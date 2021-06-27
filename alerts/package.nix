{ buildPythonApplication, isPy3k, requests}:

buildPythonApplication rec {
  pname = "alert-forward-server";
  version = "0.1";
  src = ./.;
  propagatedBuildInputs = [ requests ];
  disabled = !isPy3k;
}
