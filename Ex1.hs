module Ex1 where
    import System.IO
        

    imprimenome :: IO()
    imprimenome = do
            putStrLn "Qual o seu nome?"
            nome <- getLine
            conteudo <- readFile "nomes.txt"
            encontranome nome (lines conteudo)
    
    encontranome :: String -> [String] -> IO()
    encontranome nome [] = putStrLn ("O nome " ++nome++ "não foi encontrado")
    encontranome nome (linha:linhas) = do
                        if head(words linha) == nome
                            then putStrLn linha
                            else encontranome nome linhas

    imprimesegundonome :: IO()
    imprimesegundonome = do
            putStrLn "Qual o teu nome?"
            name <- getLine
            content <- readFile "nomes.txt"
            encontrasegundonome name (lines content)

    encontrasegundonome :: String -> [String] -> IO()
    encontrasegundonome name [] = putStrLn ("O nome " ++name++ "não foi encontrado")
    encontrasegundonome name (li:lis) = do
                                if head(words li) == name
                                    then putStrLn (words li !! 1)
                                    else encontrasegundonome name lis