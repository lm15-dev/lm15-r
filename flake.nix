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
            packages = with pkgs.rPackages; [ jsonlite curl openssl filelock later httpuv processx xml2 testthat knitr rmarkdown tibble purrr ];
          };
        in {
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
