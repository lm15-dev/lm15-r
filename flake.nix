{
  description = "Reproducible R development and offline verification for lm15";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      eachSystem = nixpkgs.lib.genAttrs systems;
    in {
      devShells = eachSystem (system:
        let
          pkgs = import nixpkgs { inherit system; };
          wsCurl = pkgs.curl.override { websocketSupport = true; };
          r = pkgs.rWrapper.override {
            packages = with pkgs.rPackages; [ jsonlite curl openssl askpass later httpuv processx xml2 callr pkgload testthat knitr rmarkdown ];
          };
          # Everything `R CMD check --as-cran` runs on CRAN's side: the PDF
          # manual (LaTeX with inconsolata), HTML validation and the
          # configure-script portability check.
          tex = pkgs.texliveSmall.withPackages (ps: with ps; [ inconsolata fancyvrb ec cm-super ]);
        in rec {
          cran = default.overrideAttrs (old: {
            nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ tex pkgs.html-tidy pkgs.checkbashisms pkgs.qpdf pkgs.ghostscript ];
          });
          default = pkgs.mkShell {
            # All R extensions in this shell must resolve the same, WebSocket-
            # enabled libcurl, even if R's HTTP module is loaded first.
            LD_LIBRARY_PATH = "${pkgs.lib.getLib wsCurl}/lib";
            DYLD_LIBRARY_PATH = "${pkgs.lib.getLib wsCurl}/lib";
            packages = [ r pkgs.python3 pkgs.git pkgs.nodejs pkgs.pkg-config wsCurl.dev pkgs.pandoc ]
              ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [ pkgs.podman pkgs.iproute2 pkgs.util-linux ];
          };
        });
    };
}
