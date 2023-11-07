http-reverse-proxy HTTP 103 early hints repro
==================

This repro shows how `http-reverse-proxy` doesn't work properly in the presence of [HTTP 103 early hints](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/103).

## Starting it up

``` shell
# Start a simple Node.js Express server on port 3005 which serves two routes,
# /simple and /early_hints
npm install
npm run start
```

In a separate terminal:

``` shell
# Start an http-reverse-proxy app on port 3006, which proxies to localhost:3005
stack run
```

## Observe that CURLing directly against the Node.js server works

``` shell
curl -v localhost:3005/simple
*   Trying 127.0.0.1:3005...
* Connected to localhost (127.0.0.1) port 3005 (#0)
> GET /simple HTTP/1.1
> Host: localhost:3005
> User-Agent: curl/7.81.0
> Accept: */*
>
* Mark bundle as not supporting multiuse
< HTTP/1.1 200 OK
< X-Powered-By: Express
< Content-Type: text/html; charset=utf-8
< Content-Length: 16
< ETag: W/"10-uViC3pa4b5/U2h5JkCXIHUyMvk0"
< Date: Tue, 07 Nov 2023 01:29:04 GMT
< Connection: keep-alive
< Keep-Alive: timeout=5
<
* Connection #0 to host localhost left intact
Simple response!%
```

``` shell
curl -v localhost:3005/early_hints
*   Trying 127.0.0.1:3005...
* Connected to localhost (127.0.0.1) port 3005 (#0)
> GET /early_hints HTTP/1.1
> Host: localhost:3005
> User-Agent: curl/7.81.0
> Accept: */*
>
* Mark bundle as not supporting multiuse
< HTTP/1.1 103 Early Hints
< Link: </some-file.js>
* Mark bundle as not supporting multiuse
< HTTP/1.1 200 OK
< X-Powered-By: Express
< Content-Type: text/html; charset=utf-8
< Content-Length: 21
< ETag: W/"15-EIUAtwHBL8Aa4+FoWIXD+4EwqTQ"
< Date: Tue, 07 Nov 2023 01:29:25 GMT
< Connection: keep-alive
< Keep-Alive: timeout=5
<
* Connection #0 to host localhost left intact
Early hints response!%
```

## Observe that the early hints request hangs when using `http-reverse-proxy`

``` shell
curl -v localhost:3006/simple
*   Trying 127.0.0.1:3006...
* Connected to localhost (127.0.0.1) port 3006 (#0)
> GET /simple HTTP/1.1
> Host: localhost:3006
> User-Agent: curl/7.81.0
> Accept: */*
>
* Mark bundle as not supporting multiuse
< HTTP/1.1 200 OK
< Transfer-Encoding: chunked
< Server: Warp/3.3.20
< X-Powered-By: Express
< Content-Type: text/html; charset=utf-8
< ETag: W/"10-uViC3pa4b5/U2h5JkCXIHUyMvk0"
< Date: Tue, 07 Nov 2023 01:30:03 GMT
< Connection: keep-alive
< Keep-Alive: timeout=5
<
* Connection #0 to host localhost left intact
Simple response!%
```

The proxied request above to `/simple` worked. But if we do a request to `/early_hints`, the connection hangs for 60 seconds and then the server closes the connection. CURL reports "Empty reply from server".

``` shell
time curl -v localhost:3006/early_hints
*   Trying 127.0.0.1:3006...
* Connected to localhost (127.0.0.1) port 3006 (#0)
> GET /early_hints HTTP/1.1
> Host: localhost:3006
> User-Agent: curl/7.81.0
> Accept: */*
>
* Mark bundle as not supporting multiuse
< HTTP/1.1 103 Early Hints
< Date: Tue, 07 Nov 2023 01:31:54 GMT
< Server: Warp/3.3.20
< Link: </some-file.js>
* Empty reply from server
* Closing connection 0
curl: (52) Empty reply from server
curl -v localhost:3006/early_hints  0.01s user 0.00s system 0% cpu 1:00.03 total
```
