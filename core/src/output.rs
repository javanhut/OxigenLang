//! Where `print`, `println`, `log` and `input`'s prompt write.
//!
//! Standard output, unless the host installs a sink. A host that shows a
//! script's output somewhere else (a game console, a web page, the website
//! generator checking documentation examples) installs one, and it then
//! receives every piece of text scripts on this thread print. The sink is per
//! thread, like a `VM`: tasks spawned with `diverge` still print to standard
//! output.

use std::cell::RefCell;
use std::io::Write;

/// Receives each piece of text a script prints, newlines included.
pub type Sink = Box<dyn FnMut(&str)>;

thread_local! {
    static SINK: RefCell<Option<Sink>> = const { RefCell::new(None) };
}

/// Send this thread's script output to `sink`, or back to standard output
/// with `None`. Returns the sink it replaces.
pub fn set_sink(sink: Option<Sink>) -> Option<Sink> {
    SINK.with(|s| std::mem::replace(&mut *s.borrow_mut(), sink))
}

/// Write `text` where scripts on this thread print: the sink, or standard output.
pub fn write(text: &str) {
    // A sink that prints from inside itself would find SINK borrowed; that
    // text goes to standard output rather than panicking.
    let sent = SINK.with(|s| match s.try_borrow_mut() {
        Ok(mut sink) => sink.as_mut().map(|f| f(text)).is_some(),
        Err(_) => false,
    });
    if !sent {
        // `print` has no trailing newline, so flush or streaming output never appears.
        // A closed stdout (`oxigen x.oxi | head`) is not the script's error; it
        // was ignored the same way before output moved here from builtins.rs.
        let mut out = std::io::stdout().lock();
        let _ = out.write_all(text.as_bytes());
        let _ = out.flush();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::rc::Rc;

    #[test]
    fn sink_receives_output_until_removed() {
        let seen = Rc::new(RefCell::new(String::new()));
        let into = Rc::clone(&seen);
        set_sink(Some(Box::new(move |text| into.borrow_mut().push_str(text))));
        write("a");
        write("b\n");
        assert!(set_sink(None).is_some());
        assert_eq!(*seen.borrow(), "ab\n");
    }
}
