FROM haskell:8.10.7

COPY . /app

WORKDIR /app

RUN stack build --fast

EXPOSE 8000
CMD stack run