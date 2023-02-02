{...} :
{
  programs.ssh = {
    enable = true;
    extraConfig = ''
  Host *
    IgnoreUnknown UseKeychain
    UseKeychain yes
    AddKeysToAgent yes
    IdentityFile ~/.ssh/id_rsa
    '';
  };
}
