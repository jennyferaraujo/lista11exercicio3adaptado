      $set sourceformat"free"
      *>Divisão de identificação do programa
       identification division.
       program-id. "lista11exercicio3adaptado".
       author. "Jennyfer Araujo".
       installation. "PC".
       date-written. 14/07/2020.
       date-compiled. 14/07/2020.

      *>Divisão para configuração do ambiente
       environment division.
       configuration section.
           special-names. decimal-point is comma.

      *>-----Declaração dos recursos externos
       input-output section.
       file-control.

           select arqAlunos assign to "arqAlunos.dat"
           organization is indexed
           access mode is dynamic
           lock mode is automatic
           record key is fd-cod
           file status is ws-fs-arqAlunos.

       i-o-control.

      *>Declaração de variáveis
       data division.

      *>----Variaveis de arquivos
       file section.
       fd  arqAlunos.
       01  fd-alunos.
           05  fd-cod                              pic 9(03).
           05  fd-aluno                            pic x(25).
           05  fd-endereco                         pic x(35).
           05  fd-mae                              pic x(25).
           05  fd-pai                              pic x(25).
           05  fd-telefone                         pic x(15).
           05  fd-nota-g.
               10  fd-notas occurs 4.
                   15 fd-nota                      pic 9(02)v99.

      *>----Variaveis de trabalho
       working-storage section.

       77  ws-fs-arqAlunos                         pic 9(02).

       01 ws-msn-erro.
          05 ws-msn-erro-ofsset                    pic 9(04).
          05 filler                                pic x(01) value "-".
          05 ws-msn-erro-cod                       pic 9(02).
          05 filler                                pic x(01) value space.
          05 ws-msn-erro-text                      pic x(42).

       01  ws-alunos.
           05  ws-cod                              pic 9(03).
           05  ws-aluno                            pic x(25).
           05  ws-endereco                         pic x(35).
           05  ws-mae                              pic x(25).
           05  ws-pai                              pic x(25).
           05  ws-telefone                         pic x(15).
           05  ws-nota-g.
               10  ws-notas occurs 4.
                   15  ws-nota                     pic 9(02)v99.

       01  ws-geral.
           05  ws-sair                             pic x(01).
           05  ws-menu                             pic x(01).
           05  ws-ind-nota                         pic 9(01).

      *>----Variaveis para comunicação entre programas
       linkage section.

      *>----Declaração de tela
       screen section.

      *>Declaração do corpo do programa
       procedure division.

           perform inicializa.
           perform processamento.
           perform finaliza.

      *>------------------------------------------------------------------------
      *>  procedimentos de inicialização
      *>------------------------------------------------------------------------
       inicializa section.

           open i-o arqAlunos   *> open i-o abre o arquivo para leitura e escrita
           if ws-fs-arqAlunos  <> 00
           and ws-fs-arqAlunos <> 05 then
               move 1                                to ws-msn-erro-ofsset
               move ws-fs-arqAlunos                  to ws-msn-erro-cod
               move "Erro ao abrir arq. arqAlunos "  to ws-msn-erro-text
               perform finaliza-anormal
           end-if

           .
       inicializa-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Processamento Principal
      *>------------------------------------------------------------------------
       processamento section.

           perform until ws-sair = "X"
                      or ws-sair = "x"

               display erase
               display "'1' - Novo Cadastro de Aluno"
               display "'2' - Registro de Notas"
               display "'3' - Consulta Cadastro"
               display "'4' - Alteracao de Cadastro"
               display "'5' - Exclusão de Cadastro"
               accept ws-menu

               evaluate ws-menu
                   when = "1"
                       perform cadastrar-aluno

                   when = "2"
                       perform cadastrar-notas

                   when = "3"
                       perform consultar-cadastro

                   when = "4"
                       perform alterar-cadastro

                   when = "5"
                       perform deletar-cadastro

                   when other
                       display "Opcao Invalida!"

               end-evaluate

               display "Informe 'X' para sair"
               accept ws-sair

           end-perform

           .
       processamento-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  cadastro de aluno
      *>------------------------------------------------------------------------
       cadastrar-aluno section.

           display "Informe o Cod do Aluno: "
           accept ws-cod
           display "Informe o Nome do Aluno: "
           accept ws-aluno
           display "Informe o Endereco do Aluno: "
           accept ws-endereco
           display "Informe o Nome da Mae: "
           accept ws-mae
           display "Informe o Nome do Pai: "
           accept ws-pai
           display "Informe o Telefone: "
           accept ws-telefone

           write fd-alunos   from   ws-alunos
           if ws-fs-arqAlunos  <> 00 then
               move 1                                  to ws-msn-erro-ofsset
               move ws-fs-arqAlunos                    to ws-msn-erro-cod
               move "Erro ao gravar arq. arqAlunos "   to ws-msn-erro-text
               perform finaliza-anormal
           end-if

           .
       cadastrar-aluno-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  cadastro de notas
      *>------------------------------------------------------------------------
       cadastrar-notas section.

           display "Cod. do Aluno: "
           accept ws-cod

           display "Nota 1: "
           accept ws-nota(1)

           display "Nota 2: "
           accept ws-nota(2)

           display "Nota 3: "
           accept ws-nota(3)

           display "Nota 4: "
           accept ws-nota(4)

           move ws-cod     to    fd-cod
           read arqAlunos
           if ws-fs-arqAlunos  <> 00 then
               if ws-fs-arqAlunos = 23 then
                   display "Cod. Aluno Inexistente!"
               else
                   move 1                                  to ws-msn-erro-ofsset
                   move ws-fs-arqAlunos                    to ws-msn-erro-cod
                   move "Erro ao ler arq. arqAlunos "      to ws-msn-erro-text
                   perform finaliza-anormal
               end-if
           else
               move ws-nota-g   to    fd-nota-g
               rewrite fd-alunos
               if ws-fs-arqAlunos  <> 00 then
                   move 1                                        to ws-msn-erro-ofsset
                   move ws-fs-arqAlunos                          to ws-msn-erro-cod
                   move "Erro ao gravar notas arq. arqAlunos "   to ws-msn-erro-text
                   perform finaliza-anormal
               end-if
           end-if

           .
       cadastrar-notas-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  consultar cadastro
      *>------------------------------------------------------------------------
       consultar-cadastro section.

           display "Informe 'I' para consulta indexada"
           display "Informe 'S' para consulta sequencial"
           accept ws-menu

           evaluate ws-menu
               when = "I"
                   perform consulta-indexada

               when = "S"
                   perform consulta-sequencial-next

               when other
                  display "Opcao Invalida"
           end-evaluate

           .
       consultar-cadastro-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  consulta indexada
      *>-----------------------------------------------------------------------
       consulta-indexada section.

           display "Informe o Cod. Do Aluno: "
           accept ws-cod

           move ws-cod         to    fd-cod
           read arqAlunos
           if ws-fs-arqAlunos  <> 00 then
               if ws-fs-arqAlunos = 23 then
                   display "Cod. Aluno Inexistente!"
               else
                   move 1                                  to ws-msn-erro-ofsset
                   move ws-fs-arqAlunos                    to ws-msn-erro-cod
                   move "Erro ao ler arq. arqAlunos "      to ws-msn-erro-text
                   perform finaliza-anormal
               end-if
           else
               move fd-alunos     to   ws-alunos

               display "Aluno   : "    ws-aluno
               display "Mae     : "    ws-mae
               display "Pai     : "    ws-pai
               display "Endereco: "    ws-endereco
               display "Tel.    : "    ws-telefone
               display "Nota 1  : "    ws-nota(1)
               display "Nota 2  : "    ws-nota(2)
               display "Nota 3  : "    ws-nota(3)
               display "Nota 4  : "    ws-nota(4)
           end-if

           .
       consulta-indexada-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  consulta sequencial
      *>-----------------------------------------------------------------------
       consulta-sequencial-next section.

           perform until ws-sair = "N"
                      or ws-sair = "n"
               read arqAlunos next
               if ws-fs-arqAlunos  <> 00 then
                   if ws-fs-arqAlunos = 10 then
                       perform consulta-sequencial-prev
                   else
                       move 1                                  to ws-msn-erro-ofsset
                       move ws-fs-arqAlunos                    to ws-msn-erro-cod
                       move "Erro ao ler arq. arqAlunos "      to ws-msn-erro-text
                       perform finaliza-anormal
                   end-if
               else
                   move fd-alunos     to   ws-alunos

                   display "Aluno   : "    ws-aluno
                   display "Mae     : "    ws-mae
                   display "Pai     : "    ws-pai
                   display "Endereco: "    ws-endereco
                   display "Tel.    : "    ws-telefone
                   display "Nota 1  : "    ws-nota(1)
                   display "Nota 2  : "    ws-nota(2)
                   display "Nota 3  : "    ws-nota(3)
                   display "Nota 4  : "    ws-nota(4)
               end-if

               display "Ler Proximo Cadastro 'S'im ou 'N'ao"
               accept ws-sair
           end-perform

           .
       consulta-sequencial-next-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  consulta sequencial previous
      *>-----------------------------------------------------------------------
       consulta-sequencial-prev section.

           perform until ws-sair = "N"
                      or ws-sair = "n"
               read arqAlunos previous
               if ws-fs-arqAlunos  <> 00 then
                   if ws-fs-arqAlunos = 10 then
                       perform consulta-sequencial-next
                   else
                       move 1                                  to ws-msn-erro-ofsset
                       move ws-fs-arqAlunos                    to ws-msn-erro-cod
                       move "Erro ao ler arq. arqAlunos "      to ws-msn-erro-text
                       perform finaliza-anormal
                   end-if
               else
                   move fd-alunos     to   ws-alunos

                   display "Aluno   : "    ws-aluno
                   display "Mae     : "    ws-mae
                   display "Pai     : "    ws-pai
                   display "Endereco: "    ws-endereco
                   display "Tel.    : "    ws-telefone
                   display "Nota 1  : "    ws-nota(1)
                   display "Nota 2  : "    ws-nota(2)
                   display "Nota 3  : "    ws-nota(3)
                   display "Nota 4  : "    ws-nota(4)
               end-if

               display "Ler Proximo Cadastro 'S'im ou 'N'ao"
               accept ws-sair
           end-perform

           .
       consulta-sequencial-prev-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  alterar cadastro
      *>-----------------------------------------------------------------------
       alterar-cadastro section.

           display "Informe o Cod do Aluno"
           accept ws-cod

           move ws-cod    to   fd-cod
           read arqAlunos
           if ws-fs-arqAlunos  <> 00 then
               if ws-fs-arqAlunos = 23 then
                   display "Cod. Aluno Inexistente!"
               else
                   move 1                                  to ws-msn-erro-ofsset
                   move ws-fs-arqAlunos                    to ws-msn-erro-cod
                   move "Erro ao ler arq. arqAlunos "      to ws-msn-erro-text
                   perform finaliza-anormal
               end-if
           else

               move fd-alunos     to    ws-alunos

               display "'1' para Aluno"
               display "'2' para Endereco"
               display "'3' para nome da Mae"
               display "'4' para nome do Pai"
               display "'5' para telefone"
               display "'6' para notas"

               accept ws-menu

               evaluate ws-menu
                   when = "1"
                       display "Nome do Aluno"
                       accept ws-aluno

                   when = "2"
                       display "Endereco"
                       accept ws-endereco

                   when = "3"
                       display "Nome Mae"
                       accept ws-mae

                   when = "4"
                       display "Nome Pai"
                       accept ws-pai

                   when = "5"
                       display "Telefone "
                       accept ws-telefone

                   when = "6"
                       display "Qual Nota (1-2-3-4)?"
                       accept ws-ind-nota
                       display "Nota : "
                       accept ws-nota(ws-ind-nota)

                   when other
                       display "Opcao Invalida"

               end-evaluate

               move ws-alunos to fd-alunos

               rewrite fd-alunos
               if ws-fs-arqAlunos  <> 00 then
                   move 1                                        to ws-msn-erro-ofsset
                   move ws-fs-arqAlunos                          to ws-msn-erro-cod
                   move "Erro ao gravar notas arq. arqAlunos "   to ws-msn-erro-text
                   perform finaliza-anormal
               end-if


           end-if

           .
       alterar-cadastro-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  deletar cadastro
      *>-----------------------------------------------------------------------
       deletar-cadastro section.
           display "Informe o Cod. Do Aluno: "
           accept ws-cod

           move ws-cod         to    fd-cod
           delete arqAlunos
           if ws-fs-arqAlunos  <> 00 then
               if ws-fs-arqAlunos = 23 then
                   display "Cod. Aluno Inexistente!"
               else
                   move 1                                  to ws-msn-erro-ofsset
                   move ws-fs-arqAlunos                    to ws-msn-erro-cod
                   move "Erro ao deletar arq. arqAlunos "  to ws-msn-erro-text
                   perform finaliza-anormal
               end-if
           end-if
           .
       deletar-cadastro-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Finalização  Anormal
      *>------------------------------------------------------------------------
       finaliza-anormal section.
           display erase
           display ws-msn-erro.
           Stop run
           .
       finaliza-anormal-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Finalização
      *>------------------------------------------------------------------------
       finaliza section.

           close arqAlunos
           if ws-fs-arqAlunos  <> 00 then
               move 1                                  to ws-msn-erro-ofsset
               move ws-fs-arqAlunos                    to ws-msn-erro-cod
               move "Erro ao fechar arq. arqAlunos "   to ws-msn-erro-text
               perform finaliza-anormal
           end-if


           Stop run
           .
       finaliza-exit.
           exit.
