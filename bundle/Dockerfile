# Typed Imp tutorial - build-from-source fallback: an image with `spectecx`
# plus asciidoctor, so participants need no host toolchain. Run from this
# directory, with it mounted at /work:
#
#   docker build -t spectecx-tutorial .
#   docker run --rm -v "$PWD":/work spectecx-tutorial \
#     spectecx impty batch --spec impty.spectec --batch-dir tests/base
#   docker run --rm -v "$PWD":/work spectecx-tutorial make splice-html
#
# Override the revision with --build-arg SPECTECX_REF=<tag-or-branch>.

FROM ocaml/opam:debian-12-ocaml-5.1

# opam deps need GMP (bignum); asciidoctor needs Ruby.
USER root
RUN apt-get update && apt-get install -y --no-install-recommends \
      libgmp-dev pkg-config m4 \
      ruby ruby-dev build-essential \
 && gem install --no-document asciidoctor asciidoctor-pdf \
 && rm -rf /var/lib/apt/lists/*
USER opam

ARG SPECTECX_REPO=https://github.com/kaist-plrg/spectecx.git
ARG SPECTECX_REF=main

RUN git clone --depth 1 --branch "$SPECTECX_REF" "$SPECTECX_REPO" /home/opam/spectecx
WORKDIR /home/opam/spectecx/spectec

RUN opam install -y --deps-only . \
 && opam exec -- dune build bin/main.exe \
 && sudo install _build/default/bin/main.exe /usr/local/bin/spectecx

WORKDIR /work
ENTRYPOINT []
CMD ["spectecx", "impty", "help"]
