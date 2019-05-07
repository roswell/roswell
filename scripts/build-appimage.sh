#!/bin/sh -ex
deploy=linuxdeploy-x86_64.AppImage
desktop=roswell.desktop
icon=roswell.png
# Download the toolchain
curl -fsSLO "https://github.com/linuxdeploy/linuxdeploy/releases/download/continuous/$deploy"
# Make it executable
chmod +x "./$deploy"
# Download the icon
curl -fsSL "https://avatars0.githubusercontent.com/u/16501222?s=256&v=4" -o "$icon"
# Build appimage
cat <<EOF > "$desktop"
[Desktop Entry]
Version=1.0
Type=Application
Name=Roswell
Comment=intend to be a lisp installer and launcher for major environment that just work.
Categories=ConsoleOnly;Development;
TryExec=ros
Exec=ros -- %F
Icon=roswell
Terminal=true
MimeType=image/png;
EOF
appdir=`mktemp -d`
./$deploy \
    --icon-file="$icon" \
    --desktop-file="roswell.desktop" \
    --appdir="$appdir" \
    --executable="$1" \
    --output=appimage
rm -rf $appdir $deploy $desktop

