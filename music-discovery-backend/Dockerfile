FROM haskell:8.10.7

COPY . /app

WORKDIR /app

# RUN stack setup && stack build --only-dependencies
# RUN stack build

RUN stack build --fast

EXPOSE 8000
CMD stack run