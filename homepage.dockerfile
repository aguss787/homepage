FROM haskell:8.6.5

RUN ["apt-get", "update"]
RUN ["apt-get", "install", "-y", "libpq-dev"]

WORKDIR homepage
COPY . .

RUN ["stack", "install"]


FROM haskell:8.6.5

RUN ["apt-get", "update"]
RUN ["apt-get", "install", "-y", "libpq-dev"]

WORKDIR /root/.local/bin/
COPY --from=0 /root/.local/bin/ .

EXPOSE 8080
CMD ["./homepage-exe"]


