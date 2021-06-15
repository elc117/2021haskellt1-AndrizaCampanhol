# 2021haskellt1-AndrizaCampanhol
2021haskellt1-AndrizaCampanhol created by GitHub Classroom

# Trabalho 1: Generative Art com Programação Funcional em Haskell

## Funcionamento:
O trabalho gera uma imagem em SVG criando dois circulos e uma escada de retângulos de maneiras variadas conforme a mudança de parâmetros. 
As variações podem ser de tamanho, raio, quantidade e cor, serão explicadas abaixo mais detalhadamente.

## Como gerar uma imagem:
Para gerar uma imagem, basta utilizar o comando ```ghci Main.hs``` para carregar o arquivo. Após, chamar a função ```main``` com os seguintes parâmetros:
  - num de retângulos -> nrects (Int)
  - inicio da variação de cor -> ini (Int)
  - tamanho da variação de cor -> vari (Int)
  - raio total dos circulos -> r (Float)
  - raio de cada circulo pequeno -> raioc (Float)
  - quantidade de circulos pequenos -> c (Int)
  - o segundo raio receberá os parâmetros a partir dos passados para o primeiro. 
  
    - **A alteração dos parâmetros deve ser feita no código para gerar imagens diferente, também pode-se trocar a paleta de cores de purplePalette para bluePalette, 
 redPalette ou rgbPalette, assim, gerando um degradê de outras cores.**
  
  ### Imagem exemplo:
  Utilizado para gerar a [Imagem](https://github.com/elc117/2021haskellt1-AndrizaCampanhol/blob/master/img.svg) de exemplo do trabalho:
  
  ```
  nrects = 24                                 --num de retangulos
  ini = 10                                    --inicio paleta de cor
  vari = 10                                   --variancia na paleta de cor
  r = 200                                     --raio total do circulo
  raioc = 40                                  --raio de cada circulo
  c = 20                                      --quantidade de circulos
  c2 = div c 2                                --quantidade de circulos no 2
  ```
  
  ## Funções de geração de **Retângulos** e **Circulos**:
  ```
 genRectsInDiagonal :: Int -> Float -> [Rect]
 genRectsInDiagonal n tam = [(((tam/2), (m*(w+gap))), w+20*m, h) | m <- [0..fromIntegral (n-1)]]
  where (w,h) = (50,50)
        gap = 5
  ```
  
  A função acima é mais simples, utiliza do tamanho da imagem e da quantidade de quadrados para gerar uma "escada" de retângulos.
  
  ```
createCirc :: Float -> Float -> Int -> Int -> String
createCirc r raioc ncirc varY = 
  printf (unlines $ ["   " ++ svgCircle ((x,y),raioc) (svgStyle (last (take (z+1) (purplePalette ncirc 60 10)))) | z <- [0..(ncirc-1)], x <- [(r + 60) + (r * (cos((fromIntegral z)*2*pi/(fromIntegral ncirc))))], y <- [(r + fromIntegral varY) + (r * (sin((fromIntegral z)*2*pi/(fromIntegral ncirc))))]])
  ```
  
  Já a função de geração de circulos acima é mais complexa, ela utiliza o raio total do circulo e os raios de cada um dos pequenos que o compõem, 
  varY é apenas o deslocamento em Y para organizá-los na imagem. As posições de cada circulo são calculadas utilizando o seno e o coseno, vou deixar um [gráfico](https://cdn.discordapp.com/attachments/693239632946790470/853822449728356362/funcoes-trigonometricas-2.png) 
  para demonstrar a maneira de obter os pontos mais visualmente.
