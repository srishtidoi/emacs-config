# [[file:config.org::*Extra Requirements][Extra Requirements:1]]
cargo install git-delta
# Extra Requirements:1 ends here

# [[file:config.org::*Extra Requirements][Extra Requirements:2]]
cargo install cargo-script
# Extra Requirements:2 ends here

# [[file:config.org::*\[\[https:/github.com/zachcurry/emacs-anywhere\]\[Emacs Anywhere\]\] configuration][[[https://github.com/zachcurry/emacs-anywhere][Emacs Anywhere]] configuration:1]]
cd /tmp
curl -fsSL https://raw.github.com/zachcurry/emacs-anywhere/master/install -o ea-install.sh
sed -i 's/EA_PATH=$HOME\/.emacs_anywhere/EA_PATH=$HOME\/.local\/share\/emacs_anywhere/' ea-install.sh
bash ea-install.sh || exit
cd ~/.local/share/emacs_anywhere
# Install in ~/.local not ~/.emacs_anywhere
sed -i 's/$HOME\/.emacs_anywhere/$HOME\/.local\/share\/emacs_anywhere/' ./bin/linux ./bin/emacstask
ln -s ~/.local/share/emacs_anywhere/bin/linux ~/.local/bin/emacs_anywhere
# Improve paste robustness --- https://github.com/zachcurry/emacs-anywhere/pull/66
sed -i 's/xdotool key --clearmodifiers ctrl+v/xdotool key --clearmodifiers Shift+Insert/' ./bin/linux
# [[https://github.com/zachcurry/emacs-anywhere][Emacs Anywhere]] configuration:1 ends here

# [[file:config.org::*Hunspell][Hunspell:1]]
cd /tmp
curl -o "hunspell-en-custom.zip" 'http://app.aspell.net/create?max_size=80&spelling=GBs&spelling=AU&max_variant=0&diacritic=keep&special=hacker&special=roman-numerals&encoding=utf-8&format=inline&download=hunspell'
unzip "hunspell-en-custom.zip"

sudo chown root:root en-custom.*
sudo mv en-custom.{aff,dic} /usr/share/myspell/
# Hunspell:1 ends here

# [[file:config.org::*Aspell][Aspell:1]]
cd /tmp
curl -o "aspell6-en-custom.tar.bz2" 'http://app.aspell.net/create?max_size=80&spelling=GBs&spelling=AU&max_variant=0&diacritic=keep&special=hacker&special=roman-numerals&encoding=utf-8&format=inline&download=aspell'
tar -xjf "aspell6-en-custom.tar.bz2"

cd aspell6-en-custom
./configure && make && sudo make install
# Aspell:1 ends here

# [[file:config.org::*Fetching][Fetching:2]]
go get -u gitlab.com/shackra/goimapnotify
ln -s ~/.local/share/go/bin/goimapnotify ~/.local/bin/
# Fetching:2 ends here

# [[file:config.org::*Config transcoding & service management][Config transcoding & service management:1]]
~/.config/doom/misc/mbsync-imapnotify.py
# Config transcoding & service management:1 ends here

# [[file:config.org::*Systemd][Systemd:4]]
systemctl --user enable mbsync.timer --now
# Systemd:4 ends here

# [[file:config.org::install mu from source (solus)][install mu from source (solus)]]
cd ~/.local/lib/
git clone https://github.com/djcb/mu.git
cd ./mu
./autogen.sh
make
sudo make install
# install mu from source (solus) ends here

# [[file:config.org::*Indexing/Searching][Indexing/Searching:2]]
curl --silent "https://api.github.com/repos/djcb/mu/releases/latest" | grep '"tag_name":' | sed -E 's/.*"([^"]+)".*/\1/'
mu --version | head -n 1 | sed 's/.* version //'
# Indexing/Searching:2 ends here

# [[file:config.org::*System hackery][System hackery:2]]
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

# [[file:config.org::*Sending Mail][Sending Mail:4]]
update-desktop-database ~/.local/share/applications
# Sending Mail:4 ends here

# [[file:config.org::*System config][System config:2]]
update-mime-database ~/.local/share/mime
# System config:2 ends here

# [[file:config.org::*System config][System config:3]]
xdg-mime default emacs.desktop text/org
# System config:3 ends here

# [[file:config.org::*Registering roam protocol][Registering roam protocol:2]]
xdg-mime default org-protocol.desktop x-scheme-handler/org-protocol
# Registering roam protocol:2 ends here
