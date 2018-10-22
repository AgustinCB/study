#[macro_use] extern crate failure;
extern crate futures;
extern crate hyper;

use futures::{future, Future};

use hyper::{Body, Client, Method, Request, Response, Server, StatusCode};
use hyper::client::HttpConnector;
use hyper::service::service_fn;

#[allow(unused, deprecated)]
use std::ascii::AsciiExt;

#[derive(Debug, Fail)]
enum BlockchainError {
    #[fail(display = "dummy error")]
    DummyError,
}

struct Transaction {
    sender: String,
    recipient: String,
    amount: usize,
}

struct Block {
    id: usize,
    timestamp: f64,
    transactions: Vec<Transaction>,
    proof: usize,
    previous_hash: String,
}

struct Blockchain {
    chain: Vec<Block>,
    current_transactions: Vec<Transaction>,
}

impl Blockchain {
    fn add_transaction(&mut self, t: Transaction) -> usize {
        self.current_transactions.push(t);
        self.chain.last().map(|b| b.id).unwrap_or(0)
    }
}

fn response(req: Request<Body>, _client: &Client<HttpConnector>)
    -> Box<Future<Item=Response<Body>, Error=hyper::Error> + Send>
{
    match (req.method(), req.uri().path()) {
        (&Method::GET, "/") => {
            let body = Body::from("HOLA");
            Box::new(future::ok(Response::new(body)))
        },
        _ => {
            // Return 404 not found response.
            let body = Body::from("Unknown url");
            Box::new(future::ok(Response::builder()
                                         .status(StatusCode::NOT_FOUND)
                                         .body(body)
                                         .unwrap()))
        }
    }
}

fn main() {
    let blockchain = Blockchain {
        chain: vec![],
        current_transactions: vec![],
    };
    let addr = "127.0.0.1:9999".parse().unwrap();

    hyper::rt::run(future::lazy(move || {
        // Share a `Client` with all `Service`s
        let client = Client::new();

        let new_service = move || {
            // Move a clone of `client` into the `service_fn`.
            let client = client.clone();
            service_fn(move |req| {
                response(req, &client)
            })
        };

        let server = Server::bind(&addr)
            .serve(new_service)
            .map_err(|e| eprintln!("server error: {}", e));

        println!("Listening on http://{}", addr);

        server
    }));
}
