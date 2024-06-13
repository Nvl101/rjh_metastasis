# running shiny app with
# usage: runapp.sh [r_app path] [port_no]
script_file=$1
shiny_port=$2
shiny_host=0.0.0.0
log_path_prefix=/var/log/shinyapp_

if  ! [ -f $script_file ]; then
    echo -e "file not found: $1";
    exit 1;
fi
R -e "shiny::runApp('$1', host='$shiny_host', port=$shiny_port)"|tee "$log_path_prefix$script_file.log"
