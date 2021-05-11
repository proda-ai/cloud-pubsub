# Google Cloud PubSub Client for Haskell (Internal Prototype Stage)

This library provides a client to publish and consume Google Cloud PubSub messages through Google's [REST API](https://cloud.google.com/pubsub/docs/reference/rest).

The library provides functions to access to most PubSub endpoints, with the notable omission of IAM permissions related endpoints.

On top of the basic endpoints there is a higher-level producer that is capable of batching messages (which helps reduce costs and improve throughput) transparently to the user.

There is a plan to offer a higher-level message processor to pull messages from a subscription, to process them and then acknowledge them.

## License
This software is under the BSD 3 License.
