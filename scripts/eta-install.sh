# From: https://github.com/haskell-works/eta-kafka-client/blob/master/scripts/eta-install.sh
#!/usr/bin/env bash
if [ ! -d "${HOME}/eta" ]; then
    echo "Installing ETA"
    cd ${HOME}
    git clone --recursive https://github.com/typelead/eta
    cd eta
    ./install.sh
else
    echo "Updating ETA"
    cd ${HOME}/eta
    _pull=$(git pull)
    git submodule sync
    _subs=$(git submodule update --recursive)

    if [ "$_pull" == "Already up-to-date." ] && [ -z "$_subs" ]; then
  	    echo "Already up-to-date."
  	    exit 0
    fi

    ./cleaninstall.sh
    etlas update
fi
