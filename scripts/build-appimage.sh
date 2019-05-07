#!/bin/sh -ex
cd `dirname $0`
cd ..
deploy=appimagetool-x86_64.AppImage
desktop=roswell.desktop
appdir=roswell.AppDir
icon=roswell.png
./configure prefix=`pwd`/$appdir
make
make install
# Download the toolchain
curl -fsSLO "https://github.com/AppImage/AppImageKit/releases/download/11/$deploy"
# Make it executable
chmod +x "./$deploy"
cd $appdir
# Download the icon
curl -fsSL "https://avatars0.githubusercontent.com/u/16501222?s=256&v=4" -o "$icon"
curl -fsSL "https://raw.githubusercontent.com/AppImage/AppImageKit/master/resources/AppRun" -o AppRun
chmod +x AppRun

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
cd ..
./$deploy roswell.AppDir