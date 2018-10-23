extern crate crypto;
#[macro_use] extern crate failure;
extern crate futures;
extern crate hyper;
extern crate serde;
#[macro_use] extern crate serde_derive;
extern crate serde_json;

use crypto::digest::Digest;
use crypto::sha2::Sha256;

use failure::Error;

use futures::{future, Future, Stream};

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

fn is_valid_proof(new: usize, last: usize) -> bool {
    let s = format!("{}{}", new, last);
    let mut digest = Sha256::new();
    digest.input(s.as_bytes());
    let hash = digest.result_str();
    hash
        .chars()
        .rev()
        .take(4)
        .fold(true, |acc, val| acc && (val == '0'))
}

fn new_proof(last_proof: usize) -> usize {
    let mut result = 0;
    while !is_valid_proof(result, last_proof) {
        result += 1;
    }
    result
}

#[derive(Debug, Fail)]
enum BlockchainError {
    #[fail(display = "No genesis block")]
    NoGenesisBlock,
}

#[derive(Clone, Debug, Hash, Serialize, Deserialize)]
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
        let string = serde_json::to_string(self).map_err(|e| Error::from(e))?;
        let mut digest = Sha256::new();
        digest.input(string.as_bytes());
        Ok(digest.result_str())
    }
}

struct Blockchain {
    chain: Vec<Block>,
    current_transactions: Vec<Transaction>,
}

impl Blockchain {
    fn new() -> Result<Blockchain, Error> {
        let mut b = Blockchain {
            chain: vec![],
            current_transactions: vec![],
        };
        b.new_block(Some((100, "1".to_owned())))?;
        Ok(b)
    }

    fn add_transaction(&mut self, t: Transaction) -> Result<usize, Error> {
        self.current_transactions.push(t);
        self.chain.last().map(|b| b.id).ok_or(Error::from(BlockchainError::NoGenesisBlock))
    }

    fn new_block(&mut self, options: Option<(usize, String)>) -> Result<&Block, Error> {
        let (proof, previous_hash) = if let Some((p, h)) = options {
            (p, h)
        } else {
            let block = self.chain.last().ok_or(Error::from(BlockchainError::NoGenesisBlock))?;
            (new_proof(block.proof), block.hash()?)
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
            let body = Body::from("pong");
            Box::new(future::ok(Response::new(body)))
        },
        (&Method::POST, "/transaction") => {
            let p = req.into_body().map(|chunk| {
                let s = std::str::from_utf8(chunk.into_iter().collect::<Vec<u8>>().as_slice()).unwrap().to_owned();
                let t: Transaction = serde_json::from_str(&s).unwrap();
                t
            }).collect();
            let body = Body::from("ok");
            Box::new(p.then(|_| future::ok(Response::new(body))))
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
    let addr = "127.0.0.1:9999".parse().unwrap();

    hyper::rt::run(future::lazy(move || {
        let blockchain = Blockchain::new().unwrap();
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
