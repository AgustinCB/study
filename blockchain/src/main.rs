#[macro_use] extern crate failure;
extern crate futures;
extern crate hyper;
extern crate serde;
#[macro_use] extern crate serde_derive;
extern crate serde_json;

use failure::Error;

use futures::{future, Future};

use hyper::{Body, Client, Method, Request, Response, Server, StatusCode};
use hyper::client::HttpConnector;
use hyper::service::service_fn;

#[allow(unused, deprecated)]
use std::ascii::AsciiExt;
use std::time::{SystemTime, UNIX_EPOCH};

fn get_timestamp() -> Result<u64, Error> {
    let start = SystemTime::now();
    Ok(start.duration_since(UNIX_EPOCH)?.as_secs())
}

#[derive(Debug, Fail)]
enum BlockchainError {
    #[fail(display = "No genesis block")]
    NoGenesisBlock,
}

#[derive(Clone, Serialize, Deserialize)]
struct Transaction {
    sender: String,
    recipient: String,
    amount: usize,
}

#[derive(Serialize, Deserialize)]
struct Block {
    id: usize,
    timestamp: u64,
    transactions: Vec<Transaction>,
    proof: usize,
    previous_hash: String,
}

impl Block {
    pub(crate) fn hash(&self) -> Result<String, Error> {
        serde_json::to_string(self).map_err(|e| Error::from(e))
    }
}

struct Blockchain {
    chain: Vec<Block>,
    current_transactions: Vec<Transaction>,
}

impl Blockchain {
    fn add_transaction(&mut self, t: Transaction) -> Result<usize, Error> {
        self.current_transactions.push(t);
        self.chain.last().map(|b| b.id).ok_or(Error::from(BlockchainError::NoGenesisBlock))
    }

    fn new_block(&mut self, proof: usize, previous_hash: Option<String>) -> Result<&Block, Error> {
        let previous_hash = if let Some(h) = previous_hash {
            h
        } else {
            let block = self.chain.last().ok_or(Error::from(BlockchainError::NoGenesisBlock))?;
            block.hash()?
        };
        let block = Block {
            id: self.chain.len() + 1,
            timestamp: get_timestamp()?,
            transactions: self.current_transactions.clone(),
            proof,
            previous_hash,
        };
        self.current_transactions = vec![];
        self.chain.push(block);
        Ok(self.chain.last().unwrap())
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
