{ mkDerivation, base, containers, irc-client, lens, protolude
, stdenv, text
}:
mkDerivation {
  pname = "stallmanbot";
  version = "0.0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers irc-client lens protolude text
  ];
  license = stdenv.lib.licenses.gpl3;
}
