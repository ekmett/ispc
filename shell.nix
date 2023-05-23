let
  # Merge into master of https://github.com/NixOS/nixpkgs/pull/252177
  nixpkgsSrc = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/56a65bbad598126ac081392250d183bedee4d406.tar.gz"; #
    sha256 = "sha256:03v3y42f8zm3bzrldl3i9yrqc1r1kc65mx50hcc12wp3b5f0xlmf";
  };
in

with (import nixpkgsSrc {}); mkShell {
  buildInputs = [ ispc zlib ];
}
