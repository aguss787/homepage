FROM haskell:8.6.5

COPY homepage/ homepage/
WORKDIR homepage

RUN ["stack", "update"]
CMD ["stack", "run"]