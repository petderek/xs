#lang racket
(require yaml)

; aliases 
(define labels hash)
(define annotations hash)

(struct K8S (apiVersion kind metadata spec) #:transparent)
(define (K8S->yaml contents)
  (hash "apiVersion" (K8S-apiVersion contents)
          "kind" (K8S-kind contents)
          "metadata" (K8S-metadata contents)
          "spec" (K8S-spec contents)))

(struct ConfigMap K8S (data) #:transparent)

(define (configmap name config)
  (ConfigMap "v1"
       "ConfigMap"
       (hash "name" name)
       'null
       (hash "data" config)))

(struct port (listen target) #:transparent)
(define (portlist .rest) (for/list ([i rest] [j rest]) (port i j)))

(define (ports->svc portlist)
  (map (lambda (i)
         (hash "port" (port-listen i)
               "targetPort" (port-target i)))
       portlist))

(define (service name selector ports)
  (K8S "v1"
       "Service"
       (hash "name" name)
       (hash "selector"
             (hash "name"
                   (hash "app.kubernetes.io/name" selector))
             "ports" (ports->svc ports))))
  

(define (write-k8s document)
  (write-yaml
   (K8S->yaml document)
   #:explicit-start? #t
   #:style 'block))
 

(write-k8s
 (configmap "mymap"
            (hash "nginx" "mine")))
(write-k8s
 (service "myservice"
          "myselector"
          (list (port 80 null))))
          