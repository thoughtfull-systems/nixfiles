{ lib, ... } : {
  "Gdk/WindowScalingFactor" = lib.mkDefault 1;
  "Gtk/ButtonImages" = lib.mkDefault true;
  "Gtk/CanChangeAccels" = lib.mkDefault false;
  "Gtk/CursorThemeName" = lib.mkDefault "Adwaita";
  "Gtk/CursorThemeSize" = lib.mkDefault 16;
  "Gtk/DialogsUseHeader" = lib.mkDefault false;
  "Gtk/FontName" = lib.mkDefault "B612 11";
  "Gtk/MenuImages" = lib.mkDefault true;
  "Gtk/MonospaceFontName" = lib.mkDefault "Source Code Pro 11";
  "Net/DndDragThreshold" = lib.mkDefault 8;
  "Net/DoubleClickDistance" = lib.mkDefault 5;
  "Net/DoubleClickTime" = lib.mkDefault 400;
  "Net/EnableEventSounds" = lib.mkDefault false;
  "Net/EnableInputFeedbackSounds" = lib.mkDefault false;
  "Net/IconThemeName" = lib.mkDefault "Adwaita";
  "Net/ThemeName" = lib.mkDefault "Adwaita";
  "Xfce/LastCustomDPI" = lib.mkDefault 96;
  "Xft/Antialias" = lib.mkDefault 1;
  "Xft/DPI" = lib.mkDefault 96;
  "Xft/HintStyle" = lib.mkDefault "hintfull";
  "Xft/RGBA" = lib.mkDefault "none";
}
