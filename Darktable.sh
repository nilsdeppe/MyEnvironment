#!/bin/bash -e
# Build darktable 5.6.2 for macOS with JPEG XL DNG support (rawspeed PR #971),
# packaged as an app bundle the same way the official release is.

DT_TAG=release-5.6.2
APP_DEST=/Applications/darktable-jxl.app

# --- 1. Homebrew dependencies (darktable's .ci/Brewfile at 5.6.2) ------------
brew install coreutils cmake pkg-config adwaita-icon-theme desktop-file-utils \
  exiv2 gettext git glib gmic gphoto2 gtk+3 gtk-mac-integration icu4c \
  graphicsmagick imagemagick intltool iso-codes jpeg-turbo jpeg-xl json-glib \
  lensfun libarchive libavif libheif libraw librsvg libsecret libsoup \
  little-cms2 lua@5.4 libomp ninja onnxruntime openexr openjpeg osm-gps-map \
  portmidi potrace pugixml sdl2 sdl3 shared-mime-info curl perl webp
brew link --force lua@5.4 libsoup libomp

# --- 2. Rebuild GraphicsMagick without loadable modules (as the release does) -
# Skipped if already done: the stock build has a separate modules folder.
if ls -d "$(brew --prefix)"/lib/GraphicsMagick*/modules* >/dev/null 2>&1; then
  export HOMEBREW_NO_INSTALL_FROM_API=1
  brew developer on
  f=$(brew edit --quiet --print-path graphicsmagick)
  sed -i '' 's/--with-modules/--disable-installed/' "$f"
  brew reinstall --build-from-source --force graphicsmagick
  unset HOMEBREW_NO_INSTALL_FROM_API
fi

# --- 3. Source: darktable release + rawspeed PR #971 --------------------------
[ -d darktable ] || git clone https://github.com/darktable-org/darktable.git
cd darktable
git checkout "$DT_TAG" && git submodule update --init
RS=src/external/rawspeed
git -C "$RS" fetch https://github.com/darktable-org/rawspeed.git pull/971/head
# git -C "$RS" log --oneline HEAD..FETCH_HEAD     # should list exactly 9 PR commits
git -C "$RS" merge --no-edit FETCH_HEAD

# --- 4. Configure, build, install into build/macosx (where packaging expects it)
export MACOSX_DEPLOYMENT_TARGET=15.0

rm -rf ./build
cmake -S . -B build -G Ninja \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_INSTALL_PREFIX="$PWD/build/macosx" \
  -DBINARY_PACKAGE_BUILD=ON \
  -DBUILD_CURVE_TOOLS=ON \
  -DBUILD_NOISE_TOOLS=ON \
  -DUSE_GRAPHICSMAGICK=ON \
  -DUSE_IMAGEMAGICK=OFF \
  -DUSE_AI=ON \
  -DWITH_JPEGXL=ON \
  -DTESTBUILD_OPENCL_PROGRAMS=OFF

cmake --build build
cmake --install build

# --- 5. Make the .app bundle (official packaging step) ------------------------
packaging/macosx/3_make_hb_darktable_package.sh
APP=build/macosx/package/darktable.app

# --- 6. D-Bus fix: point GLib at a nonexistent bus so startup doesn't hang ----
# LSEnvironment applies however the app is launched (Finder, Dock, open).
PB=/usr/libexec/PlistBuddy
$PB -c "Delete :LSEnvironment" "$APP/Contents/Info.plist" 2>/dev/null || true
$PB -c "Add :LSEnvironment dict" \
    -c "Add :LSEnvironment:DBUS_SESSION_BUS_ADDRESS string unix:path=/nonexistent" \
    "$APP/Contents/Info.plist"
codesign --force --deep -s - "$APP"    # re-sign after editing Info.plist

# --- 7. Install --------------------------------------------------------------
rm -rf "$APP_DEST"
cp -R "$APP" "$APP_DEST"
/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -f "$APP_DEST"

cd ..
rm -rf ./darktable

echo "Installed $APP_DEST"
