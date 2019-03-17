{ reflex-platform ? import ./reflex-platform {} }:

reflex-platform.project ({ pkgs, ... }: {
  packages = {
    common = ./common;
    backend = ./backend;
    frontend = ./frontend;
    reflex-chartjs = ./reflex-chartjs;
  };

  android.frontend = {
    executableName = "frontend";
    applicationId = "org.example.frontend";
    displayName = "Example Android App";
  };

  ios.frontend = {
    executableName = "frontend";
    bundleIdentifier = "org.example.frontend";
    bundleName = "Example iOS App";
  };

  shells = {
    ghc = ["common" "backend" "frontend" "reflex-chartjs"];
    ghcjs = ["common" "frontend" "reflex-chartjs"];
  };
})
