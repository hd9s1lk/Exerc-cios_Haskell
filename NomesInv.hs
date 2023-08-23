module NomesInv where
        gravanomesinv :: IO()
        gravanomesinv = do
            conteudo <- readFile "nomes.txt"
            writeFile "nomecontrario.txt" "Nomes: \n"
            gravanome (lines conteudo)
        
        gravanome :: [String] -> IO()
        gravanome [] = return ()
        gravanome (nome:nomes) = do
            appendFile "nomecontrario.txt" (last(words nome) ++ ", " ++ head (words nome))
            gravanome nomes
