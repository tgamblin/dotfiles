OS=`uname -s`;
if [ "$OS" = 'Darwin' ]; then
    alias bounce-on='defaults write com.apple.dock no-bouncing -bool FALSE; killall Dock'
    alias bounce-off='defaults write com.apple.dock no-bouncing -bool TRUE; killall Dock'
fi
