FROM ubuntu:latest
COPY target/scala-2.12/raicoin-assembly-0.1.0-SNAPSHOT.jar .
RUN apt-get update && apt-get install -y openjdk-11-jre
ARG runtime_args
ENV RUNTIME_ARGS $runtime_args
CMD java -jar raicoin-assembly-0.1.0-SNAPSHOT.jar ${RUNTIME_ARGS}
