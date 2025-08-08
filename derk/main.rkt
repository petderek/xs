#lang racket
(require racket/hash)
(require yaml)


;; util
(define (as-list in) (if (list? in) in (list in)))

(define current-namespace (make-parameter #f))
(define current-labels (make-parameter #f))
(define current-selector (make-parameter #f))
(define current-annotations (make-parameter #f))

(define-syntax-rule (with-annotations next body ...)
  (let* ([current (current-annotations)]
         [old (if current current (hash))]
         [merged (hash-union old next)])
    (parameterize ([current-annotations merged])
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

(define (to-spec obj)
  (cond
    [(list? obj) (map to-spec obj)]
    [(to-spec? obj) ((to-spec-ref obj) obj)]
    [else (error obj)]))

(struct K8S (apiVersion kind metadata) #:transparent
  #:property prop:to-spec (lambda (self) (error "not supported")))
(define (K8S->yaml contents)
  (hash "apiVersion" (K8S-apiVersion contents)
        "kind" (K8S-kind contents)
        "metadata" (K8S-metadata contents)
        "spec" (to-spec contents)))

(define (metadata [name #f])
  (hash-filter-values
   (hash "name" name
         "annotations" (current-annotations)
         "namespace" (current-namespace)
         "labels" (current-labels))
   (lambda (v) v)))

(define (named-metadata name)
  (hash-union (hash "name" name) (metadata)))

(struct ConfigMap K8S (data) #:transparent
  #:property prop:to-spec (lambda (self) (ConfigMap-data self)))

(define (configmap name config)
  (ConfigMap "v1"
             "ConfigMap"
             (named-metadata name)
             (hash "data" config)))

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

(struct Container (name image ports) #:transparent
  #:property prop:to-spec
  (lambda (self) (hash "name" (Container-name self)
                       "image" (Container-image self)
                       "ports" (as-list (to-spec (Container-ports self))))))

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

(define (container name image ports)
  (PodTemplate #f (list (Container name image ports))))

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
                

(define cert-annotations (hash "cert-manager.io/issuer" "letsencrypt"))
(define auth-annotations (hash "nginx.ingress.kubernetes.io/auth-url" "test"
                               "nginx.ingress.kubernetes.io/auth-signin" "test"))

(compile-to-yaml
 (with-namespace "default"
   (with-annotations cert-annotations
     (with-annotations auth-annotations
       (ingress "excalidraw" "nginx"
                (IngressRule "example.com" (IngressPath "/" "Exact" (IngressBackend "excalidraw" 80))) (IngressTLS "host" "secret"))))
   (service "excalidraw" (Port 80 80))
   (deployment "excalidraw"
               (container "excalidraw" "excalidraw/excalidraw:latest" (ContainerPort 80 10999)))))