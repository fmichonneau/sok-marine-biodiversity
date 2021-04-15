## The state of knowledge for marine invertebrate biodiversity in the continental US

Work in progress, aiming at synthesizing the publicly available data about marine invertebrate biodiversity within the continental US.

### Build Docker container

```shell
docker build -t sok:latest .
```

### Run container

``` shell
sudo docker run --rm -it -v /home/francois/sok-marine-biodiversity/:/sok -v /home/francois/sok-marine-biodiversity/db/data:/var/lib/postgresql/data sok:latest /bin/bash
```

