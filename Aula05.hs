module Aula05 where
    
    type Jornada = [Jogo]
    type Jogo = ((Equipa,Golos), (Equipa,Golos))
    type Equipa = String
    type Golos = Int

    igualj :: Jornada -> Bool
    igualj l = foldr (\((e1,_),(e2,_)) c -> (e1 /= e2)&&c) True l

    equipas :: Jornada -> [Equipa]
    equipas [] = []
    equipas l = foldr (\((e1,_),(e2,_)) c -> e1:e2:c) [] l
    



