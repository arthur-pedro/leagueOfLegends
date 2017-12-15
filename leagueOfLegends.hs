-- Funções do programa: Cria, Edita, Apaga, procura ou Compra
-- Estes dados estão armazenados em um arquivo.

-- importa as bibliotecas necessárias 
import Prelude
import System.IO
import System.Exit
--import Data.List.Splitc


data Campeao = Empty | Campeao String Int String String Bool deriving (Show, Read, Eq)
type Database = [Campeao]

data Usuario = Nulo | Usuario String String deriving (Show, Read, Eq)
type Userbase = [Usuario]

-- BANCO DE DADOS

-- cria a database
createDatabase :: IO ()
createDatabase = saveDatabase []

-- ler a database
readDatabase :: IO Database
readDatabase = do
    handle <- openFile "database.txt" ReadMode
    linha <- hGetLine handle
    database <- (readIO linha)::IO Database
    hClose handle
    return database

-- salvar a database

saveDatabase :: Database -> IO ()
saveDatabase x = do
    handle <- openFile "database.txt" WriteMode
    hPrint handle x
    hClose handle

-- SISTEMA DE LOGIN

createUserbase :: IO ()
createUserbase = saveUserbase []


-- ler a database
readUserbase :: IO Userbase
readUserbase = do
    handle <- openFile "usuarios.txt" ReadMode
    linha <- hGetLine handle
    database <- (readIO linha)::IO Userbase
    hClose handle
    return database

-- salvar a database
saveUserbase :: Userbase -> IO ()
saveUserbase x = do
    handle <- openFile "usuarios.txt" WriteMode
    hPrint handle x
    hClose handle

cadastrar :: Usuario -> IO () 
cadastrar (Usuario nick senha) = do
	w <- check (nick)
	if w 
		then putStrLn "\n nick ja cadastrado"
		else do 
			novoUsuario <- readUserbase
			saveUserbase (novoUsuario++[(Usuario nick senha)])

verificaNick :: String -> IO Usuario
verificaNick j = do
    novoLogin <- readUserbase
    let l = (filter (\(Usuario nick senha) -> nick == j) novoLogin)
    if l == []
        then return (Nulo)
        else do 
            return (head l)

logarInteractive = do
    getNick >>= verificaNick >>= autenticaNick
    
getNick = do
    putStr "\n"
    putStr "Digite seu nick: "
    getLine
    

autenticaNick (Usuario nick senha) = do
    if (Usuario nick senha) == Nulo
        then putStrLn "\n  Usuário ou senha incorreto"

        else do
            logarInteractive_2
            

verificaSenha :: String -> IO Usuario
verificaSenha j = do
    novoLogin <- readUserbase
    let l = (filter (\(Usuario nick senha) -> senha == j) novoLogin)
    if l == []
        then return (Nulo)
        else do 
            return (head l)

logarInteractive_2 = do
    getSenha >>= verificaSenha >>= autenticaSenha
    
getSenha = do
    putStr "\n"
    putStr "Digite sua senha: "
    getLine 

autenticaSenha (Usuario nick senha) = do
    if (Usuario nick senha) == Nulo
        then putStrLn "\n  Usuário ou senha incorreto"

        else do
            putStrLn "Login efetuado com sucesso!"
            menu
            

cadastrarInteractive = do
    (a,b) <- getUsuario
    cadastrar (Usuario a b)
    main

getUsuario = do
    putStr "\n"
    putStr "Digite seu nick: "
    a <- getLine
    putStr "Digite sua senha: "
    b <- getLine
    return (a,b) 


            

-- CAMPEÔES

-- Insere um campeão
inserir :: Campeao -> IO ()
inserir (Campeao nome preco funcao reino habilitado) = do
    p <- check (nome)
    if p
        then putStrLn "\n  Campeao já Habilitado!"
        else do 
            newData <- readDatabase
            saveDatabase (newData++[(Campeao nome preco funcao reino habilitado)])

-- Função de removerr um Pokemon
remover :: String -> IO ()
remover n = do
    p <- check (n)
    if p == False 
        then putStrLn "\n  Impossivel apagar, Campeao não existe!"
        else do
            newData <- readDatabase
            saveDatabase (filter (aux) newData)
            where 
            aux (Campeao nome preco funcao reino habilitado) = nome /= n

-- Função de procurar um Pokemon
procurar :: String -> IO Campeao
procurar p = do
    newData <- readDatabase
    let l = (filter (\(Campeao nome preco funcao reino habilitado) -> nome == p) newData)
    if l == []
        then return (Empty)
        else do 
            return (head l)

-- Função de atualizar um Pokemon
atualizar :: Campeao -> IO ()
atualizar (Campeao nome preco funcao reino habilitado)  = do
    p <- check (nome)
    if p == False 
        then putStrLn "\n  Impossivel atualizar, Campeao inexistente!"
        else do
            remover (nome)
            inserir (Campeao nome preco funcao reino habilitado)
            putStrLn "\n  Campeao atualizado com sucesso!"
atualizar a = return ()

-- Função de capturar um Pokemon
habilitar :: String -> IO()
habilitar nome = do
    p <- check (nome)
    if p == False 
        then putStrLn "\n  Impossivel Habilitar, Campeao inexistente!"
        else do
            (Campeao nome preco funcao reino habilitado) <- procurar nome
            if habilitado
                then putStrLn "\n  Campeao já habilitado!"
                else do
                    remover (nome)
                    inserir (Campeao nome preco funcao reino True)
                    putStrLn "\n  Campeao habilitado com sucesso!"

-- Função de validacao
check :: String -> IO Bool
check s = do
    p <- procurar (s)
    if p == Empty
        then return False
        else do
            return True

-- INTERAÇÃO COM USUÁRIO
 
-- Função que converde um IO em inteiro
getInt :: IO Int
getInt = do
    line <- getLine
    return (read line :: Int)

-- printa um Campeão retornando o valor procurado na tela
printCampeao (Campeao nome preco funcao reino habilitado) = do
    if (Campeao nome preco funcao reino habilitado) == Empty
        then putStrLn "\n  Campeao inexistente :("
        else do
            putStr "\n"
            putStr "  Nome: "
            putStrLn nome
            putStr "  Preço: "
            putStrLn (show preco)
            putStr "  Função: "
            putStrLn funcao
            putStr "  reino: "
            putStrLn reino
            putStr "  habilitado: "
            if habilitado == True 
                then putStrLn "Sim"
                else do
                    putStrLn "Nao"

-- Função que recebe o nome de um Pokemon do usuario
getNome = do
    putStr "\n"
    putStr "Digite nome: "
    getLine

-- Função que recebe um Pokemon inteiro do usuario
getCampeao = do
    putStr "\n"
    putStr "Digite nome: "
    a <- getLine
    putStr "Digite Preço: "
    b <- getInt
    putStr "Digite o(s) Função(s): "
    c <- getLine
    putStr "Digite reino: "
    d <- getLine
    return (a,b,c,d) 

-- Função de insercao interativo
inserirInteractive = do
    (a,b,c,d) <- getCampeao
    inserir (Campeao a b c d False)
    menu

-- Função de remocao interativo
removerInteractive = do
    getNome >>= remover
    menu

-- Função de atualizacao interativo
atualizarInteractive = do
    (a,b,c,d) <- getCampeao
    atualizar (Campeao a b c d False)
    menu

-- Função de procura interativo
procurarInteractive = do
    getNome >>= procurar >>= printCampeao
    menu

-- Função de captura interativo
habilitarInteractive = do
    getNome >>= habilitar
    menu

-- Função lagout

lagout = do
	putStrLn "\n        ---ADEUS INVOCADOR---"
	main

-- MENU
menu :: IO()
menu = do 
		
        putStr "\n"
        putStrLn "<================================>"
        putStrLn "  BEM-VINDOS A LEAGUE OF LEGENDS  "
        putStrLn "<================================>"
        putStrLn "|      1 - Criar Campeao         |"
        putStrLn "|      2 - Editar Campeao        |"
        putStrLn "|      3 - Apagar Campeao        |"
        putStrLn "|      4 - Procurar Campeao      |"
        putStrLn "|      5 - Habilitar Campeao     |"
        putStrLn "|      6 - Lagout                |"
        putStrLn "|      0 - Sair                  |"
        putStrLn "<================================> "
        putStrLn "<================================>"
        putStrLn "\n"
        putStr "    Insira uma opcao: "
        opcao <- getLine
        checkOption (opcao)
              
checkOption :: String -> IO ()
checkOption opcao 
    |opcao == "1" = inserirInteractive
    |opcao == "2" = atualizarInteractive
    |opcao == "3" = removerInteractive
    |opcao == "4" = procurarInteractive
    |opcao == "5" = habilitarInteractive
    |opcao == "6" = lagout
    |otherwise = exitFailure

-- LOGIN
main :: IO()
main = do 
        putStr "\n"
        putStrLn "<================================>"
        putStrLn "        BEM VINDO INVOCADOR       "
        putStrLn "<================================>"
        putStrLn "|          1 - cadastrar         |"
        putStrLn "|          2 - login             |"
        putStrLn "|          0 - Sair              |"
        putStrLn "<================================>"
        putStrLn "<================================>"
        putStrLn "\n"
        putStr "    Insira uma opcao: "
        opcao_login <- getLine
        checkOption_login (opcao_login)
              
checkOption_login :: String -> IO ()
checkOption_login opcao_login 
    |opcao_login == "1" = cadastrarInteractive
    |opcao_login == "2" = logarInteractive
    |otherwise = exitFailure

