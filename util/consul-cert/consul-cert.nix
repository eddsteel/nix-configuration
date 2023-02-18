{ stdenv, openssl, domain, consulMain, writeTextFile }:
let
  subj = "/C=CA/ST=BC/L=Vancouver/O=/OU=/CN=localhost/";
  openssl-conf = writeTextFile {
    name = "openssl.conf";
    text = ''
      [req]
      req_extensions = req_ext
      distinguished_name = dn
      [ dn ]
      CN = ${consulMain}.${domain}
      [ req_ext ]
      basicConstraints=CA:FALSE
      subjectAltName = @alt_names
      [ alt_names ]
      DNS.1 = localhost
      DNS.2 = ${consulMain}.${domain}.local
      IP.1  = 127.0.0.1
      IP.2  = 0.0.0.0
    '';
  };
in stdenv.mkDerivation {
  name = "consul-cert";
  src = ./.;
  pname = "consul-cert";
  buildPhases = ["installPhase"];
  buildDepends = [openssl];
  installPhase = ''
    # From https://iotech.force.com/edgexpert/s/article/secure-consul-tls
    mkdir tmp
    mkdir $out
    # Prepare
    ${openssl}/bin/openssl req \
      -newkey rsa:2048 -new -nodes -x509 -days 3650 \
      -subj '${subj}' \
      -keyout tmp/key.pem \
      -out tmp/ca.pem
    # Create CSR
    ${openssl}/bin/openssl req \
      -new -config ${openssl-conf} -newkey rsa:2048 -nodes \
      -subj '/CN=${consulMain}.${domain}' \
      -keyout tmp/server.key -out tmp/server.csr
    # Sign
    ${openssl}/bin/openssl x509 -req -extfile ${openssl-conf} \
      -extensions req_ext -in tmp/server.csr \
      -CA tmp/ca.pem -CAkey tmp/key.pem -CAcreateserial \
      -out tmp/server.crt
    # Grab the files we care about
    chmod 644 tmp/server.key
    mv tmp/{ca.pem,server.crt,server.key} $out
 '';
}
