# Build
FROM rust:1.74 as builder
WORKDIR /usr/src/rlox

COPY . .
RUN cargo build --release


# Run
FROM rust:1.74
WORKDIR /root/
COPY --from=builder /usr/src/rlox/target/release/rlox .

CMD ["./rlox"]