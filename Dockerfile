FROM ubuntu:latest
COPY target/scala-2.12/raicoin-assembly-0.1.0-SNAPSHOT.jar .
RUN apt-get update && apt-get install -y openjdk-11-jre
ARG node_type
ENV NODE_TYPE $node_type
CMD java -jar raicoin-assembly-0.1.0-SNAPSHOT.jar --${NODE_TYPE}
