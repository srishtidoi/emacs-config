# [[file:~/.config/doom/config.org::*Extra Requirements][Extra Requirements:1]]
cargo install git-delta
# Extra Requirements:1 ends here

# [[file:~/.config/doom/config.org::*Extra Requirements][Extra Requirements:2]]
cargo install cargo-script
# Extra Requirements:2 ends here

# [[file:~/.config/doom/config.org::*Fetching][Fetching:2]]
go get -u gitlab.com/shackra/goimapnotify
ln -s ~/go/bin/goimapnotify ~/.local/bin/
# Fetching:2 ends here

# [[file:~/.config/doom/config.org::*Config transcoding & service management][Config transcoding & service management:1]]
~/.config/doom/misc/mbsync-imapnotify.py
# Config transcoding & service management:1 ends here

# [[file:~/.config/doom/config.org::*Systemd][Systemd:4]]
systemctl --user enable mbsync.timer --now
# Systemd:4 ends here

# [[file:~/.config/doom/config.org::install mu from source (solus)][install mu from source (solus)]]
cd ~/.local/lib/
git clone https://github.com/djcb/mu.git
cd ./mu
./autogen.sh
make
sudo make install
# install mu from source (solus) ends here

# [[file:~/.config/doom/config.org::*Indexing/Searching][Indexing/Searching:2]]
curl --silent "https://api.github.com/repos/djcb/mu/releases/latest" | grep '"tag_name":' | sed -E 's/.*"([^"]+)".*/\1/'
mu --version | head -n 1 | sed 's/.* version //'
# Indexing/Searching:2 ends here

# [[file:~/.config/doom/config.org::*System hackery][System hackery:2]]
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
# System hackery:2 ends here

# [[file:~/.config/doom/config.org::*Sending Mail][Sending Mail:4]]
update-desktop-database ~/.local/share/applications
# Sending Mail:4 ends here

# [[file:~/.config/doom/config.org::*System config][System config:2]]
update-mime-database ~/.local/share/mime
# System config:2 ends here

# [[file:~/.config/doom/config.org::*System config][System config:3]]
xdg-mime default emacs.desktop text/org
# System config:3 ends here

# [[file:~/.config/doom/config.org::*Registering roam protocol][Registering roam protocol:2]]
xdg-mime default org-protocol.desktop x-scheme-handler/org-protocol
# Registering roam protocol:2 ends here
