cargo install git-delta

cargo install cargo-script

go get -u gitlab.com/shackra/goimapnotify
ln -s ~/go/bin/goimapnotify ~/.local/bin/

~/.config/doom/misc/mbsync-imapnotify.py

systemctl --user enable mbsync.timer --now

cd ~/.local/lib/
git clone https://github.com/djcb/mu.git
cd ./mu
./autogen.sh
make
sudo make install

curl --silent "https://api.github.com/repos/djcb/mu/releases/latest" | grep '"tag_name":' | sed -E 's/.*"([^"]+)".*/\1/'
mu --version | head -n 1 | sed 's/.* version//'

cd ~/.local/lib/
git clone https://github.com/marlam/msmtp-mirror.git ./msmtp
cd ./msmtp
libtoolize --force
aclocal
autoheader
automake --force-missing --add-missing
autoconf
# if using GSASL
# PKG_CONFIG_PATH=/usr/local/lib/pkgconfig ./configure --with-libgsasl
./configure
make
sudo make install

update-desktop-database ~/.local/share/applications

update-mime-database ~/.local/share/mime

xdg-mime default emacs.desktop text/org

xdg-mime default org-protocol.desktop x-scheme-handler/org-protocol
