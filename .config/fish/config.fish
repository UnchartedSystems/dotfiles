if status is-interactive
    # Commands to run in interactive sessions can go here
end
alias dotfiles='/usr/bin/git --git-dir=/home/dan/.dotfiles/ --work-tree=/home/dan'

# set -x PGDATA /var/lib/pgsql/data
# Just in case! This fixed a nasty psql bug for me!

## For Quick VPN (andy):
# alias vpn="netExtender helios.sig-gis.com:4433 --username acarlile --domain sig-gis.com --password \$(secret-tool lookup password sig-vpn)"