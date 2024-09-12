all: docker

FILE=miss-sim

## build docker image (requires root rights for docker)
dbuild: Dockerfile
	docker build \
	-t $(FILE) .

## run docker container
docker: dbuild
	echo "open RStudio Server at http://localhost:8787"
	docker run \
	--rm \
	-ti \
	-e DISABLE_AUTH=true \
	-e ROOT=true \
	-e USERID=$(id -u) \
	-e GROUPID=$(id -g) \
	-p 8787:8787 \
	-v $(CURDIR)/case-study:/home/rstudio/case-study \
	-v $(CURDIR)/data:/home/rstudio/data \
	-v $(CURDIR)/literature-review:/home/rstudio/literature-review \
	$(FILE)
