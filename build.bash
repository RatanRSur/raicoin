#!/bin/bash

sbt assembly &&
docker-compose up --build
