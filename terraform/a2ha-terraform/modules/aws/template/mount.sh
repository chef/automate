if [ -e /hab ]; then
  echo "'hab' directory is already present"
else
  echo "Creating 'hab' directory"
  sudo mkdir -p /hab
  export DNAME=$(lsblk -o NAME,MOUNTPOINT | grep nvme[1-9] | awk 'length($2) == 0')
  echo '/dev/$DNAME  /hab xfs defaults 0 0' >> sudo /etc/fstab
  sudo mkfs -t xfs /dev/$DNAME 
  sudo mount /dev/$DNAME  /hab/
fi