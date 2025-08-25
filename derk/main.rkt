#lang racket
(require racket/hash)
(require net/base64)
(require yaml)


;; util
(define (as-list in) (if (list? in) in (list in)))

(define current-namespace (make-parameter #f))
(define current-labels (make-parameter #f))
(define current-selector (make-parameter #f))
(define current-annotations (make-parameter #f))
(define current-env (make-parameter #f))

(define-syntax-rule (with-annotations next body ...)
  (let* ([current (current-annotations)]
         [old (if current current (hash))]
         [merged (hash-union old next)])
    (parameterize ([current-annotations merged])
      (list body ...))))
(define-syntax-rule (with-env next body ...)
  (let* ([current (current-env)]
         [old (if current current (hash))]
         [merged (hash-union old next)])
    (parameterize ([current-env merged])
      (list body ...))))
(define-syntax-rule (with-namespace ns body ...)
  (parameterize ([current-namespace ns])
    (list body ...)))
(define-syntax-rule (with-labels labels body ...)
  (parameterize ([current-labels labels])
    (list body ...)))
(define-syntax-rule (bound-by-selector selector body ...)
  (parameterize ([current-selector selector])
    (list body ...)))

(define-values (prop:to-spec to-spec? to-spec-ref)
  (make-struct-type-property 'to-spec))
(define-values (prop:data-key data-key? data-key-ref)
  (make-struct-type-property 'data-key))

(define (to-spec obj)
  (cond
    [(list? obj) (map to-spec obj)]
    [(to-spec? obj) ((to-spec-ref obj) obj)]
    [else (error obj)]))

(define (data-key obj)
  (if (data-key? obj) (data-key-ref obj) (error obj)))

(struct K8S (apiVersion kind metadata) #:transparent
  #:property prop:data-key "spec"
  #:property prop:to-spec (lambda (self) (error "not supported")))
(define (K8S->yaml contents)
  (hash "apiVersion" (K8S-apiVersion contents)
        "kind" (K8S-kind contents)
        "metadata" (K8S-metadata contents)
        (data-key contents) (to-spec contents)))

(define (metadata [name #f])
  (hash-filter-values
   (hash "name" name
         "annotations" (current-annotations)
         "namespace" (current-namespace)
         "labels" (current-labels))
   (lambda (v) v)))

(define (named-metadata name)
  (hash-union (hash "name" name) (metadata)))

(define (get-name obj) (hash-ref (K8S-metadata obj) "name"))

(struct ConfigMap K8S (data) #:transparent
  #:property prop:data-key "data"
  #:property prop:to-spec (lambda (self) (ConfigMap-data self)))

(define (configmap name . rest)
  (ConfigMap "v1"
             "ConfigMap"
             (named-metadata name)
             (apply hash rest)))

(struct Port (listen target) #:transparent
  #:property prop:to-spec (lambda (self) (hash "port" (Port-listen self) "targetPort" (Port-target self))))


(define (compile-to-yaml documents)
  (cond
    [(list? documents) (for-each compile-to-yaml documents)]
    [else
     (write-yaml
      (K8S->yaml documents)
      #:explicit-start? #t
      #:explicit-end? #t
      #:style 'block)]))

(struct Selector (key value) #:transparent
  #:property prop:to-spec (lambda (self) (hash (Selector-key self) (Selector-value self))))

(struct Service K8S (selector ports) #:transparent
  #:property prop:to-spec
  (lambda (self)
    (hash "selector" (to-spec (Service-selector self))
          "ports" (as-list (to-spec (Service-ports self))))))
(struct PodTemplate (selector containers) #:transparent
  #:property prop:to-spec
  (lambda (self) (hash "metadata" (hash "labels" (to-spec (PodTemplate-selector self)))
                       "spec" (hash "containers" (as-list (map to-spec (PodTemplate-containers self)))))))

(struct ContainerPort (port host) #:transparent
  #:property prop:to-spec (lambda (self)
                            (hash "containerPort" (ContainerPort-port self)
                                  "hostPort" (ContainerPort-host self))))

(define (infer-env-key obj)
  (cond
    [(ConfigMap? obj) "envFrom"]
    [else "env"]))

(define (infer-env obj)
  (cond
    [(ConfigMap? obj) (list (hash "configMapRef" (hash "name" (get-name obj))))]
    [else 'null]))

(struct Container (name image ports env) #:transparent
  #:property prop:to-spec
  (lambda (self) (let ([envKey (infer-env-key (Container-env self))]
                       [envValue (infer-env (Container-env self))])
                   (hash "name" (Container-name self)
                         "image" (Container-image self)
                         envKey envValue
                         "ports" (as-list (to-spec (Container-ports self)))))))

(struct Deployment K8S (selector podtemplate) #:transparent
  #:property prop:to-spec
  (lambda (self)
    (hash "selector" (hash "matchLabels" (to-spec (Deployment-selector self)))
          "template" (to-spec (Deployment-podtemplate self)))))

(define (service name ports)
  (Service "v1"
           "Service"
           (named-metadata name)
           (selector-or-name name)
           ports))

(define (selector-or-name name)
  (cond
    [(current-selector) (current-selector)]
    [else (Selector "app" name)]))

(define (deployment name template)
  (let* ([selector (selector-or-name name)]
         [updatedTemplate (PodTemplate selector (PodTemplate-containers template))])
    (Deployment "apps/v1"
                "Deployment"
                (named-metadata name)
                selector
                updatedTemplate)))

(define (container name image ports env)
  (PodTemplate #f (list (Container name image ports env))))

;; ingress
(struct Ingress K8S (classname rules tls) #:transparent
  #:property prop:to-spec
  (lambda (self) (hash "ingressClassName" (Ingress-classname self)
                       "rules" (as-list (to-spec (Ingress-rules self)))
                       "tls" (as-list (to-spec (Ingress-tls self))))))

(struct IngressBackend (name port)
  #:property prop:to-spec
  (lambda (self)
    (let* ([port (IngressBackend-port self)]
           [numericalPort (number? port)]
           [kind (if numericalPort "number" "name")])
      (hash "service" (hash "name" (IngressBackend-name self)
                            "port" (hash kind port))))))

(struct IngressRule (host paths) #:transparent
  #:property prop:to-spec (lambda (self)
                            (hash "host" (IngressRule-host self)
                                  "http" (hash "paths" (as-list (to-spec (IngressRule-paths self)))))))

(struct IngressPath (path pathtype backend) #:transparent
  #:property prop:to-spec
  (lambda (self) (hash "path" (IngressPath-path self)
                       "pathType" (IngressPath-pathtype self)
                       "backend" (to-spec (IngressPath-backend self)))))

(struct IngressTLS (hosts secret) #:transparent
  #:property prop:to-spec
  (lambda (self) (let* ([hosts (IngressTLS-hosts self)]
                        [h (if (list? hosts) hosts (list hosts))])
                   (hash "hosts" h
                         "secretName" (IngressTLS-secret self)))))

(define (ingress name class rules tls)
  (Ingress "networking.k8s.io/v1"
           "Ingress"
           (metadata name)
           class
           rules
           tls))

(struct Route53Solver (region secretRef) #:transparent
  #:property prop:to-spec
  (lambda (self) (hash "dns01"
                       (hash "route53"
                             (hash "region" (Route53Solver-region self)
                                   "accessKeyIDSecretRef" (hash "name" (Route53Solver-secretRef self)
                                                                "key" "access-key-id")
                                   "secretAccessKeySecretRef" (hash "name" (Route53Solver-secretRef self)
                                                                    "key" "secret-access-key"))))))

(struct LEClusterIssuer K8S (email acmesecret solvers) #:transparent
  #:property prop:to-spec
  (lambda (self) (hash "acme"
                       (hash "server" "https://acme-v02.api.letsencrypt.org/directory"
                             "email" (LEClusterIssuer-email self)
                             "privateKeySecretRef" (hash "name" (LEClusterIssuer-acmesecret self))
                             "solvers" (as-list (to-spec (LEClusterIssuer-solvers self)))))))

(define (letsencryptroute53 name email acmesecret awsregion awssecret)
  (LEClusterIssuer "cert-manager.io/v1"
                   "ClusterIssuer"
                   (metadata name)
                   email
                   acmesecret
                   (Route53Solver awsregion awssecret)))

;; convert string to base64
(define (base64-encode-string in)
  (bytes->string/utf-8
   (base64-encode
    (string->bytes/utf-8 in)
    "")))

(struct OpaqueSecret K8S (data) #:transparent
  #:property prop:data-key "data"
  #:property prop:to-spec
  (lambda (self)
    (for/hash ([k (in-hash-keys (OpaqueSecret-data self))]
               [v (in-hash-values (OpaqueSecret-data self))])
      (values k (base64-encode-string v)))))


(define (secret name values)
  (OpaqueSecret "v1" "Secret" (metadata name) values))

;; note - you should put data in 'data.rkt' such that when executed, it returns a stream of K8S objects
;; check out 'data.rkt.example'
(compile-to-yaml (include "data.rkt"))


