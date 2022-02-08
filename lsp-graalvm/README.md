## LSP client configuration to use the GraalVM LSP support

Just exposes some GraalVM languages (JS, Ruby, Python, R) as LSP servers. The
static analysis is done by other servers, the GraalVM LSP support simply adds
some dynamic analysis features to complement the static analysis. This is
helpful when working with the GraalVM implementations since they sometimes have
additional built-ins, and when working with multiple languages using GraalVM's
polyglot support.

See https://www.graalvm.org/tools/lsp/ for a bit more background. The LSP
support was pioneered in a
[paper](https://www.hpi.uni-potsdam.de/hirschfeld/publications/media/StolpeFelgentreffHumerNiephausHirschfeld_2019_LanguageIndependentDevelopmentEnvironmentSupportForDynamicRuntimes_AcmDL_Preprint.pdf)
at DSL 2019. There's a [recording](https://www.youtube.com/watch?v=YywetLaa8rQ).

For VSCode there are more
[features](https://www.javaadvent.com/2020/12/live-programming-with-the-graalvm-the-lsp-and-vs-code.html).
