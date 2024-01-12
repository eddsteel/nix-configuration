{pkgs, lib, ...}:
{
  preference = lib.mkOptionType {
    name = "preference";
    description = "Attr set with key, value and optional type fields.";
    check = with builtins; x: isAttrs x && hasAttr "key" x && hasAttr "value" x;
  };
}
