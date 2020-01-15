#!/usr/bin/env python3
import os
os.putenv('TF_CUDNN_USE_AUTOTUNE', '0')
# Disable numpy deprecation messages
import warnings
warnings.simplefilter('ignore', category=DeprecationWarning)
warnings.simplefilter('ignore', category=FutureWarning)
# Disable tensorflow deprecation messages
import tensorflow as tf
import tensorflow.python.util.deprecation as deprecation
tf.compat.v1.logging.set_verbosity(tf.compat.v1.logging.ERROR)
deprecation._PRINT_DEPRECATION_WARNINGS = False

import argparse
import json
import numpy as np

import model, sample, encoder

import http.server

parser = argparse.ArgumentParser(
    description='Run a HTTP server proving gpt-2 sampling.',
    formatter_class=argparse.ArgumentDefaultsHelpFormatter)
parser.add_argument('--model_name', metavar='MODEL', type=str, default='117M', help='Pretrained model name')
parser.add_argument('--length', metavar='CHARS', type=int, default=10, help='Default sampling length')
parser.add_argument('--temperature', type=float, default=1.0, help='Default temperature.')
parser.add_argument('--top_k', type=int, default=40, help='Default top k sampling.')
parser.add_argument('--top_p', type=float, default=0.9, help='Default top p sampling.')
parser.add_argument('--checkpoint', type=str, help='Load checkpoint.')

def main():
    args = parser.parse_args()
    batch_size = 1
    nsamples = 1

    enc = encoder.get_encoder(args.model_name)
    hparams = model.default_hparams()
    with open(os.path.join('models', args.model_name, 'hparams.json')) as f:
        hparams.override_from_dict(json.load(f))

    if args.length > hparams.n_ctx:
        raise ValueError("Can't get samples longer than window size: %s" % hparams.n_ctx)

    config = tf.ConfigProto()
    config.gpu_options.allow_growth = True
    with tf.Session(config=config) as sess:
        context = tf.placeholder(tf.int32, [batch_size, None])
        output = sample.sample_sequence(
            hparams=hparams, length=args.length,
            context=context,
            batch_size=batch_size,
            temperature=args.temperature, top_k=args.top_k, top_p=args.top_p
        )

        saver = tf.train.Saver()
        ckpt = tf.train.latest_checkpoint(os.path.join('models', args.model_name))
        if args.checkpoint:
            ckpt = tf.train.latest_checkpoint(args.checkpoint) or args.checkpoint
        saver.restore(sess, ckpt)

        class Handler(http.server.BaseHTTPRequestHandler):
            def do_POST(self):
                print('POST', self.path)
                print(self.headers)
                length = int(self.headers['Content-Length'])
                raw_text = self.rfile.read(length).decode('utf-8')
                context_tokens = enc.encode(raw_text)
                context_size = min(len(context_tokens), hparams.n_ctx - args.length - 1)
                context_tokens = context_tokens[-context_size:]

                self.send_response(200)
                self.send_header('Content-Type', 'application/json; charset=UTF-8')
                self.send_header('Access-Control-Allow-Origin', '*')
                self.end_headers()
                out = sess.run(output, feed_dict={
                    context: [context_tokens]
                })[0, context_size:]
                text = enc.decode(out)
                print(repr(raw_text), repr(text))
                self.wfile.write(json.dumps({'text': [text], 'context': enc.decode(context_tokens)}).encode('utf-8'))
            def do_GET(self):
                print('GET', self.path)
                if self.path == '/info':
                    self.send_response(200)
                    self.send_header('Content-Type', 'application/json; charset=UTF-8')
                    self.send_header('Access-Control-Allow-Origin', '*')
                    self.end_headers()
                    info = {'model': args.model_name,
                            'checkpoint': ckpt}
                    self.wfile.write(json.dumps(info).encode('utf-8'))
                else:
                    self.send_response(404)
                    self.end_headers()

        server_address = ('', 8000)
        httpd = http.server.HTTPServer(server_address, Handler)

        print('Starting server...')
        httpd.serve_forever()

if __name__ == '__main__':
    main()
