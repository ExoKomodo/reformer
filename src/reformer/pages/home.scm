(define-module (reformer pages home)
  #:export (index))

(use-modules (web request)
             (web response)
             (web uri)
             (reformer html)
             (sxml simple)
             (reformer components navbar))

(define (index visits)
  (html-page
    `(,(navbar)
        (h1 (a (@ (href "/")) "reformer.fyi"))
        (p "Lorem ipsum odor amet, consectetuer adipiscing elit. Gravida facilisi vitae a pretium vitae accumsan. Hac at pharetra justo felis varius viverra class metus. Diam aliquet molestie nunc hac aptent, taciti lacus. Aest eget purus consequat fringilla netus vulputate lobortis ultricies. Mi natoque neque imperdiet phasellus aliquam commodo habitant.")
        (marquee ,(format #f "Visit #~d" visits)))))
