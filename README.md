### Building

- Install `opam` via your OS's package manager or another method. It is not
  necessary to install any other OCaml binaries or libraries.

- Install dependencies and build into a clean environment with:

        opam switch create fb-ingest 5.1.1
        cd fb_ingest
        # --- this will build the binary and make it available as:
        # --- "$OPAM_SWITCH_PREFIX"/bin/fb-ingest
        # --- (for example: /home/your-user/.opam/fb-ingest/bin/fb-ingest)
        opam install .

- Add

        eval $(opam env)

to your environment (e.g. .bashrc)

### Running

- Use:

        fb-ingest --help
        fb-ingest <path-to-your-csv-file>

- Use the `--no-validate` option to skip validation of the input data. This
  is for development only.
