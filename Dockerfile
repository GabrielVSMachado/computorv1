FROM haskell:9.4.8-slim

WORKDIR computorv1

RUN adduser computorv1 &&\
  chown computorv1:computorv1 -R /computorv1

COPY --chown=computorv1:computorv1 . .

User computorv1

ENTRYPOINT [ "make" ]
CMD ["run"]
