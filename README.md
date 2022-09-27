# PDX Volumetric Analyzer

[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![](https://img.shields.io/docker/pulls/sonerkoc/tumor-volume-suite.svg)](https://hub.docker.com/r/sonerkoc/tumor-volume-suite)

The PDX Volumetric Analyzer is a Shiny app for uploading, validating, plotting, and analyzing your tumor volume data.


## Overview

This repo offers the source code for the app's Docker image, including the Dockerfile and the app built. The app features PDX Volumetric Data uploading, validating, plotting, and analyzing your tumor volume data.

This `README` gives a brief introduction to pull the Docker image and run the app locally. For detailed usage of the app and more deployment options, please check our [User manual under Help page](https://tumor-volume.jax.org/).

## Installation

First of all, please make sure that Docker is installed in your system, and the `docker` commands are available from the terminal. If not, here is the [official installation guide](https://docs.docker.com/install/).

### Pull or build the image

To pull the pre-built Docker image from its [Docker Hub repo](https://hub.docker.com/repository/docker/sonerkoc/tumor-volume-suite), use:

```bash
docker pull sonerkoc/tumor-volume-suite
```

Alternatively, you can choose to build the image, which could take a few minutes:

```bash
git clone https://github.com/skoc/tumor-volume-shiny.git
cd tumor-volume-shiny
docker build . -t tumor-volume-shiny
```

### Run the container

If the image was pulled from Docker Hub, use

```bash
docker run --rm -p 3838:3838 --name sb sonerkoc/tumor-volume-suite
```

If the image was built locally, use

```bash
docker run --rm -p 3838:3838 --name sb tumor-volume-shiny
```

### Open the app

After the container is running, open http://127.0.0.1:3838 in your web browser.

### Clean up the container and image

```bash
docker rm -f sb
docker rmi sonerkoc/tumor-volume-suite
```
or

```bash
docker rm -f sb
docker rmi tumor-volume-suite
```

## LICENSE

This project is licensed under the ADD_LICENSE (LICENSE).

## AUTHORS

- [Soner Koc](https://github.com/skoc/), SevenBridges
- [Mike Lloyd](https://github.com/MikeWLloyd), The Jackson Laboratory

