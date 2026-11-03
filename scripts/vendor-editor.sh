#!/usr/bin/env bash
# Fetch the playground editor's JavaScript (CodeMirror 6 and its vim mode)
# into website/assets/vendor/, one ES module per package. The playground
# loads them through an import map (crates/oxigen-site/src/main.rs writes it
# from the files here), so there is no bundler and no node_modules.
#
# Versions and tarball checksums are pinned below; a download that does not
# match fails. The result is checked in, so the site build never touches the
# network for it. To upgrade: change a version, run with --print-hash to get
# its checksum, paste it in, run again, and try the playground.
#
#   scripts/vendor-editor.sh               fetch and verify everything
#   scripts/vendor-editor.sh --print-hash  also print each tarball's sha512
set -euo pipefail

print_hash=false
[[ ${1:-} == --print-hash ]] && print_hash=true

# package | version | its ES module entry | sha512 of the npm tarball
packages=(
    "@codemirror/autocomplete|6.20.4|dist/index.js|c85f1184e7f6497960d34faab6ea732e1ae095c6aceb56e8a9a38d88d71b3c879b1c2a49a1ca6a4c103a772aed52b40ee3b86ac9375ea1bff75e7622953ed369"
    "@codemirror/commands|6.11.1|dist/index.js|3bfe211b7482d58c1c990d1dd9454d0ecf80b1a3567758875716d378406ea87fba6c4c4088f2b7892fc1be963aad91944402efe190f7b0881c0a646fc377a106"
    "@codemirror/language|6.13.1|dist/index.js|cdfbd4dabdd3cb29502cc8ffb5efb90a7434ad2a998bc075a130816d091a81c2d6e1773a8a669beaa37ea44261b2e34e2da26a891c815e9f548560350e156f0f"
    "@codemirror/lint|6.9.7|dist/index.js|dbcffe8962d8c4ac6cbc66214982fbcda099a8bcf9f851450eaf6d56cbc6bfd92ff116387c501c849e565fd33762bad195322c1028ec5cfa9e3609d298d3f676"
    "@codemirror/search|6.7.2|dist/index.js|8146246214f6fa7ffe54667ef04cc4e56164619519626d5538a0ee748b0daa1e36b915502746ba62cb3db1d293dcc78e61fb8bd4de80640e7ba336b43b2af42c"
    "@codemirror/state|6.7.6|dist/index.js|900cfe027711b4ab8892779dc53d5bab85f05efe14a3085b9075359b23ebb556ff8d9b48996b95e415f3bf9bcaea2de46000ac76266240a15043932aede956f3"
    "@codemirror/streamparser|6.0.0|dist/index.js|25314f17ac6b7e3b749a77c63e0cdc2e2330626bc8c8d6a88b3a7c058ceff0513c2ab4fa181183675f74aedb7e3bb5a47b7806fcc147ca263d60635e8a6a1fe3"
    "@codemirror/view|6.43.15|dist/index.js|1c7b1fa6a15e1f25cbb3df37a5245edfc5da8ecd2f156650d81b97d26eca8bcc378d9e620fc72e477941cf6ff9fd7e770e17f7d920472a1eabb524c1ead29e1b"
    "@lezer/common|1.5.3|dist/index.js|1f4884ad8e1ee372e95db603c81722e56e2ffd1c1380657763360e9493e2199f1163cc39d64544fb12e7d6d83767a52649fc844efb6c50adebed8b200d0108ed"
    "@lezer/highlight|1.2.5|dist/index.js|3b518c54a82d7f962ca45691ce99aa91580cb489361c6f6e3006c7eb074021d361dd35730ac721ffe7118beea5dbffd418329ee8d07d056ddcbee4797b3310d3"
    "@marijn/find-cluster-break|1.0.4|src/index.js|5b2d15efe4865239c5f7f4e488cd612950cf8fb8ca5ddb8f35ba0c56d1d303c77248c5115aa7e0fc967d1364aaf09812266925ee4ed07bd14b78c4ab4ab69699"
    "@replit/codemirror-vim|6.4.0|dist/index.js|b7d50c0cd86499e024974b916e2255a2dbfdedb183ea67f81b2249a046eb8d4a9afcfa2fd2cf5e2fee00688d17b68dce1ab8b5ee2f6ac08a53d096d59555edf0"
    "@replit/codemirror-vim-core|0.1.0|./vim.js|d62e8404aa5c35f0cabd399387a37a83d94beae743e6dfae14d878242aa8cd19d596f5063a9b3b87f4334b69dee3372f3d48ef0292a999c3fb1ab7377cd6d989"
    "crelt|1.0.7|index.js|68ae816d67e17f853fc0270b1ca3c997fc5ae95915b2d45a3f2c16b4c286c2e38b73fc194f2418ba8c60bd99cdb01befeca83761305061804282072f890733b8"
    "style-mod|4.1.4|src/style-mod.js|5d758842deb7dff129005c7c6990ce4e3073ac26869a1bc4425428e8c54f7dad8ecced9c5a8fb8855f61fba5241d895719fcbe3835ca51d3fb3ed86672ee404f"
    "w3c-keyname|2.2.8|index.js|769a2306136c08d37b4fcd939bb936e80e86f4c2f7364843b27c3d9ff7a8c52465541078084b4843f29308b2361707f76ad6925d1858164422dd29671703ef3d"
)

root=$(cd "$(dirname "$0")/.." && pwd)
out="$root/website/assets/vendor"
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

mkdir -p "$out"
licenses="$work/LICENSES.txt"
printf 'Third-party code in this directory, fetched by scripts/vendor-editor.sh.\n' > "$licenses"

for entry in "${packages[@]}"; do
    IFS='|' read -r name version file sha <<< "$entry"
    # @scope/name -> scope-name.js, the import map's file for it.
    flat=${name#@}
    flat=${flat//\//-}
    tarball="$work/$flat.tgz"
    curl -fsSL "https://registry.npmjs.org/$name/-/${name##*/}-$version.tgz" -o "$tarball"
    got=$(sha512sum "$tarball" | cut -d' ' -f1)
    $print_hash && echo "$name $version $got"
    if [[ $got != "$sha" ]]; then
        echo "$name $version: checksum mismatch" >&2
        echo "  expected $sha" >&2
        echo "  got      $got" >&2
        exit 1
    fi
    mkdir -p "$work/$flat"
    tar -xzf "$tarball" -C "$work/$flat"
    cp "$work/$flat/package/${file#./}" "$out/$flat.js"
    {
        printf '\n── %s %s (%s.js) ──\n\n' "$name" "$version" "$flat"
        cat "$work/$flat/package/LICENSE"* 2> /dev/null || echo "MIT"
    } >> "$licenses"
done
cp "$licenses" "$out/LICENSES.txt"
echo "Vendored ${#packages[@]} packages into website/assets/vendor/."
