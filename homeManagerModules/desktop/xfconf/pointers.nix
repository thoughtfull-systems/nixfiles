{ lib, ... } : {
  "ELAN067600_04F33195_Mouse/Acceleration" = lib.mkDefault 5.0;
  "ELAN067600_04F33195_Mouse/ReverseScrolling" = lib.mkDefault false;
  "ELAN067600_04F33195_Mouse/RightHanded" = lib.mkDefault true;
  "ELAN067600_04F33195_Touchpad/Acceleration" = lib.mkDefault 5.0;
  "ELAN067600_04F33195_Touchpad/Properties/Synaptics_Edge_Scrolling" = [ 0 0 0 ];
  "ELAN067600_04F33195_Touchpad/Properties/Synaptics_Two-Finger_Scrolling" = [ 1 0 ];
  "ELAN067600_04F33195_Touchpad/Properties/libinput_Scroll_Method_Enabled" = [ 1 0 0 ];
  "ELAN067600_04F33195_Touchpad/Properties/libinput_Tapping_Enabled" = lib.mkDefault 0;
  "ELAN067600_04F33195_Touchpad/ReverseScrolling" = lib.mkDefault false;
  "ELAN067600_04F33195_Touchpad/RightHanded" = lib.mkDefault true;
  "ELAN901C00_04F32FE6/Properties/Device_Enabled" = lib.mkDefault 0;
  "TPPS2_Elan_TrackPoint/Acceleration" = lib.mkDefault 5.0;
  "TPPS2_Elan_TrackPoint/ReverseScrolling" = lib.mkDefault false;
  "TPPS2_Elan_TrackPoint/RightHanded" = lib.mkDefault true;
}
