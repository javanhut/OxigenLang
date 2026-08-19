# OxigenLang release image. The release workflow stages the prebuilt static
# musl binaries into docker-context/<arch>/ before invoking buildx, so this
# image ships the exact binaries attached to the GitHub release.
FROM alpine:3.21

ARG TARGETARCH

# /usr/local/bin/oxigen finds stdlib at /usr/local/lib/oxigen/stdlib via the
# <prefix>/bin/../lib/oxigen/stdlib rule in core/src/vm/mod.rs.
COPY docker-context/${TARGETARCH}/oxigen /usr/local/bin/oxigen
COPY stdlib/ /usr/local/lib/oxigen/stdlib/

WORKDIR /work
ENTRYPOINT ["oxigen"]
CMD ["--help"]
