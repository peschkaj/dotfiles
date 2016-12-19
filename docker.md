# Docker Configuration

Just in case you've decided to user docker...

## Configuration Files

``` shell
sudo mkdir -p /etc/systemd/system/docker.service.d 
sudo cp ~/src/peschkaj/dotfiles/docker.conf /etc/systemd/system/docker.service.d/docker.conf
sudo systemctl daemon-reload
sudo systemctl restart docker
```

## Performance

We'll also have to set up the group for docker:

``` shell
sudo groupadd docker
sudo usermod -aG docker $USER

# At this point, we need to log out and log back in.
# If we don't want to do that, we can just run the following command.
# This only works in the current terminal session, though...
newgrp docker
```

## Install a container

``` shell
docker run hello-world
```

## Using SQL Server

``` shell
docker run \ 
       -v /opt/docker/volumes/mssql-test:/var/opt/mssql \
       --name mssql-test \
       -e 'ACCEPT_EULA=Y' \
       -e 'SA_PASSWORD=P@55w0rd' \
       -p 1433:1433 \
       -d microsoft/mssql-server-linux
```


