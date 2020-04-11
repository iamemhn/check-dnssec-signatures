# Check DNSSEC signature inception and expiration

A DNSSEC signed DNS zone includes an additional *signature record*
or `RRSIG` to every name record `RR` in the zone. Each `RRSIG` has an
**inception** and an **expiration** date, the former indicating when
the signature was made and the latter when it will become invalid.
If the `RRSIG` record expires, DNSSEC-validating resolvers will
**refuse** to use the original `RR`.

A manual way to check `RR` and their `RRSIG`s is

```
$ dig +short +dnssec a iamemhn.link
107.20.149.94
A 8 2 86400 20200505151009 20200405143739 9123 iamemhn.link. yaPrQpXR47XGfziAXlqfTV/s3ZkahbpafgZFZWQUaterIgW2/uwcRhi5 nKCiTtdIIvrKyvY35yhETxxHlvYZtPheF1dv9nwlpSoo5J7GzbIlrhwL 5axRaxX7BRu/sqCJ7HwGWOodEhv2h4IQ248WA84j2LiNjsTQ7nRK0Fpm UvA=
```

This example looks for the `A` `RR` for name `iamemhn.link`,
requesting DNSSEC to retrieve both the `RR` and `RRSIG`.
The answer's first line is the response `RR`, while the second
(long) line is the accompanying `RRSIG`. The expiration date
comes first (`2020-05-05Z15:10:09`) and the inception date comes next
(`2020-04-05Z14:37:39`), just after cryptographic algorithm and checksum,
the record time-to-live information, and before the Zone Signing Key number.

Modern DNS servers like [BIND9](http://www.isc.org/bind) have ways
to ensure new `RRSIG`s are inserted regularly. Nevertheless, being
able to monitor the validity is part of a robust DNSSEC operation.

This command-line utility helps checking `RRSIG` validity
for *all available* `RR` and `RRSIG` of one or more names, typically
domain names (`iamemhn.link`) but any resolvable name
(`www.iamemhn.link`) as well. Results are presented as either
a CSV report or a JSON object describing, for each name
and *every* RR type found, the inception and expiration dates for
the `RRSIG`s, and whether or not they are soon to expire within
the requested number of days. These formats are easier to consume
for network monitoring tools.

When several domains are given as arguments, information for
each one is retrieved in a separate concurrent thread, reducing
latency and improving performance if using multiple cores.

## Check if RRSIG expire in less than a day

```
$ date --utc "+%s"
1586575240
$ check-dnssec-signatures iamemhn.link
domain,rrtype,inception,expiration,alert
iamemhn.link,A,1586097459,1588691409,no
iamemhn.link,NS,1586097459,1588691409,no
iamemhn.link,TXT,1586097459,1588691409,no
iamemhn.link,MX,1586097459,1588691409,no
iamemhn.link,NSEC,1585788478,1588382264,no
iamemhn.link,SOA,1586102683,1588698283,no
iamemhn.link,DNSKEY,1586100546,1588694932,no
iamemhn.link,DNSKEY,1586100546,1588694932,no
```

## Check if RRSIG expire in less than a given number of days

```
$ date --utc "+%s"
1586575335
$ check-dnssec-signatures -d 21 iamemhn.link
domain,rrtype,inception,expiration,alert
iamemhn.link,A,1586097459,1588691409,no
iamemhn.link,NS,1586097459,1588691409,no
iamemhn.link,TXT,1586097459,1588691409,no
iamemhn.link,MX,1586097459,1588691409,no
iamemhn.link,NSEC,1585788478,1588382264,yes
iamemhn.link,SOA,1586102683,1588698283,no
iamemhn.link,DNSKEY,1586100546,1588694932,no
iamemhn.link,DNSKEY,1586100546,1588694932,no
```

## Get the answer as a JSON object

```
$ date --utc "+%s"1586575396
emhn@trillian:~/work/check-dnssec-signatures$ stack exec check-dnssec-signatures -- -j -d 21 iamemhn.link
{
    "getResults": {
        "iamemhn.link": [
            {
                "inception": 1586097459,
                "expiration": 1588691409,
                "alert": false,
                "rrType": "A"
            },
            {
                "inception": 1586097459,
                "expiration": 1588691409,
                "alert": false,
                "rrType": "NS"
            },
            {
                "inception": 1586097459,
                "expiration": 1588691409,
                "alert": false,
                "rrType": "TXT"
            },
            {
                "inception": 1586097459,
                "expiration": 1588691409,
                "alert": false,
                "rrType": "MX"
            },
            {
                "inception": 1585788478,
                "expiration": 1588382264,
                "alert": true,
                "rrType": "NSEC"
            },
            {
                "inception": 1586102683,
                "expiration": 1588698283,
                "alert": false,
                "rrType": "SOA"
            },
            {
                "inception": 1586100546,
                "expiration": 1588694932,
                "alert": false,
                "rrType": "DNSKEY"
            },
            {
                "inception": 1586100546,
                "expiration": 1588694932,
                "alert": false,
                "rrType": "DNSKEY"
            }
        ]
    }
}
```

## Installation

After cloning, use Haskell `stack` to compile and install

```
$ stack build
$ stack exec check-dnssec-signatures -- iamemhn.link ...
$ stack install
...
Copied executables to /home/emhn/.local/bin:
- check-dnssec-signatures
```

and then use as in the examples above.

## Motivation

This is a simple proof-of-concept project showing a variety of
useful libraries within Haskell's ecosystem, as well as many
generic programming techniques, that help writing type-safe
concurrent systems programs effectively.

--

Ernesto Hernández-Novich

github@iamemhn.link
