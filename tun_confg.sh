#from mellow to work with tun2socks
DNS1=8.8.8.8
DNS2=8.8.4.4
TUN_GW=10.255.0.1
TUN_ADDR=10.255.0.2
TUN_MASK=255.255.255.0
ORIG_GW=`ip route get 1 | awk '{print $3;exit}'`
ORIG_ST_SCOPE=`ip route get 1 | awk '{print $5;exit}'`
ORIG_ST=`ip route get 1 | awk '{print $7;exit}'`




config_route() {
  # Give some time for Mellow to open the TUN device.
  ip addr add $TUN_ADDR dev tun1
  ip link set tun1 up
  sleep 3
  ip route del default table main
  ip route add default via $TUN_GW table main
  ip route add default via $ORIG_GW dev $ORIG_ST_SCOPE table default
  ip rule add from $ORIG_ST table default
  echo "Routing table is ready."
}


recover_route() {
  ip rule del from $ORIG_ST table default
  ip route del default table default
  ip route del default table main
  ip route add default via $ORIG_GW table main
  echo "Routing table recovered."
}

pause_read() {
    echo 'press enter key to continue'
    read
}

config_route
pause_read
recover_route
