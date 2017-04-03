package DutyWork

/**
*  Copyright@lEo of George Washington University
  */

import scala.io._

class TwentyOnePointGame {
  val Poker = new Array[Int](52)
  //扑克的编号
  val PokerName = new Array[String](52)
  //扑克的名字
  val PokerPoint = new Array[Int](52)
  //扑克的点数
  val Suits = Array("Spades", "Hearts", "Diamonds", "Clubs")
  //花色
  var Dealer = new Array[Int](5)
  //庄家指电脑手里的牌 为程序简单 此处用的扑克牌的编号而不是牌名
  var Gamer = new Array[Int](5)
  //闲家指玩家手里的牌
  var NumOfGamerPoker: Int = 0
  //玩家手里的牌数
  var NumOfDealerPoker: Int = 0
  //电脑手里的牌数
  var BlackJackChoose : Int = 1


  //因为我们的类不需要参数 其实那些初始化的for循环应该写进构造函数的,但是构造数组遇到问题,放弃治疗了
  /**
    * 初始化编号
    */

  for (i <- 0 until Poker.length) {
    Poker(i) = i
  }

  /**
    * 初始化扑克名
    */
  for (j <- 0 until Suits.length) {
    PokerName(j * 13) = Suits(j) + "A"
    PokerName(j * 13 + 1) = Suits(j) + "2"
    PokerName(j * 13 + 2) = Suits(j) + "3"
    PokerName(j * 13 + 3) = Suits(j) + "4"
    PokerName(j * 13 + 4) = Suits(j) + "5"
    PokerName(j * 13 + 5) = Suits(j) + "6"
    PokerName(j * 13 + 6) = Suits(j) + "7"
    PokerName(j * 13 + 7) = Suits(j) + "8"
    PokerName(j * 13 + 8) = Suits(j) + "9"
    PokerName(j * 13 + 9) = Suits(j) + "10"
    PokerName(j * 13 + 10) = Suits(j) + "J"
    PokerName(j * 13 + 11) = Suits(j) + "Q"
    PokerName(j * 13 + 12) = Suits(j) + "K"
  }

  /**
    * 初始化扑克的点数
    */
  for (i <- 0 until PokerPoint.length) {
    if (i % 13 < 9) {
      PokerPoint(i) = i % 13 + 1
    }
    else {
      PokerPoint(i) = 10
    }
  }

  for (i <- 0 until Gamer.length) {
    Gamer(i) = -1
    Dealer(i) = -1
  }


  /**
  *   开始游戏
    */
  def BeginNewGame(): Unit =
  {
    /**
    *   各发两张牌
      */
    var AlreadyDeal : Boolean = false                                                    //判断牌是否已发
    var random : Int = -1
    for(i <- 0 until 4) {
      AlreadyDeal = false
      while(!AlreadyDeal) {                                                    //如果此牌已发 生成一张新的牌
        random = (new util.Random()).nextInt(52)
        if (!Dealer.contains(random) && !Gamer.contains(random)) {             //判断牌有木有已经发给玩家了,一副扑克不可能有两张一样的
          if(i % 2 == 0)                                                       //第一步和第三步给庄家发 第二步和第四步给闲家发
            {
              Dealer(i/2) = random
            }
          if(i % 2 == 1)
            {
              Gamer(i/2) = random
            }
            AlreadyDeal = true
        }
      }
    }
    NumOfDealerPoker = 2
    NumOfGamerPoker = 2
  }

  /**
  *   玩家要牌
    */
  def GamerAskForPoker()= {
    if (NumOfGamerPoker == 5) { //玩家手牌已满
      println("你的手牌已满")
    }
    else
    {
      var AlreadyDeal = false  //判断牌是否已发
      var random : Int = -1
      while (!AlreadyDeal) {
        //如果此牌已发 生成一张新的牌
        random = (new util.Random()).nextInt(52)
        if (!Dealer.contains(random) && !Gamer.contains(random)) {
          Gamer(NumOfGamerPoker) = random
          NumOfGamerPoker += 1
          AlreadyDeal = true
        }
      }
    }
  }

    def DealerAskForPoker() = {
      if (NumOfDealerPoker == 5) //电脑手牌已满
      {
        println("电脑手牌已满")
      }
      else {
        var AlreadyDeal = false //判断牌是否已发
        var random : Int = -1
        while (!AlreadyDeal) {
          //如果此牌已发 生成一张新的牌
          random = (new util.Random()).nextInt(52)
          if (!Dealer.contains(random) && !Gamer.contains(random)) {
            Dealer(NumOfDealerPoker) = random
            NumOfDealerPoker += 1
            AlreadyDeal = true
          }
        }
      }
    }

    /**
      *返回玩家手牌数
      */
    def getNumOfGamerPoker : Int = this.NumOfGamerPoker


    /**
      *返回电脑手牌数
      */
    def getNumOfDealerPoker : Int = this.NumOfDealerPoker

    def setBlackJackChoose(BlackjackChoose : Int) : Unit = this.BlackJackChoose=BlackjackChoose

    def GamerContainBlackJack : Boolean = {
      val PointOfGamer = {(for (i <- 0 to NumOfGamerPoker - 1) yield PokerPoint(Gamer(i))).sum}
      var ContainBlackJack = false
      for (i <- 0 to NumOfGamerPoker - 1) {
        if (Gamer(i) % 13 == 0) // BlackJack
        {
          ContainBlackJack = true
        }
      }
      return ContainBlackJack
    }


    /**
      *返回玩家的分数 这两个函数是新增的
      */
    def getPointOfGamer : Int = {
      val PointOfGamer = {(for (i <- 0 to NumOfGamerPoker - 1) yield PokerPoint(Gamer(i))).sum}
      if (BlackJackChoose == 1) {
        return PointOfGamer
      }
      else
        {
          return PointOfGamer + 10
        }
    }

    /**
      *返回电脑的分数 这两个函数是新增的
      */

    def getPointOfDealer : Int = {
      val PointOfDealer = {(for (i <- 0 to NumOfDealerPoker - 1) yield PokerPoint(Dealer(i))).sum}
      var ContainBlackJack = false
      for (i <- 0 to NumOfDealerPoker - 1)
      {
        if (Dealer(i) % 13 == 0)           // BlackJack
        {
          ContainBlackJack = true
        }
      }
      if (ContainBlackJack == true) {
        if (PointOfDealer + 10 <= 21) {
          return PointOfDealer + 10
        }
        else {
          return PointOfDealer
        }
      }
      else
      {
        return PointOfDealer
      }
    }


    /**------------------------------------------------------------
    *  第二版这里我们把判断是否爆牌的两个函数删除了 你可以删掉--以内的这一段注释部分
      */

      /**
      *判断玩家是否爆牌

    //不知道爆牌的英语怎么说 你翻译一下
    def 玩家是否爆牌的英语 : Boolean = {
      if (getPointOfGamer > 21) return true
      else return false
    }

    def 电脑是否爆牌的英语 : Boolean = {
      if (getPointOfDealer > 21) return true
      else return false
    }--------------------------------------------------------------*/

    def WhoWinTheGame(): Unit = {

      if (getPointOfGamer > 21 && getPointOfDealer <= 21) {
        BotWin(getPointOfGamer,getPointOfDealer)
      }
      else if (getPointOfGamer > 21 && getPointOfDealer > 21) {
        if (getPointOfGamer < getPointOfDealer) {
          YouWin(getPointOfGamer,getPointOfDealer)
        }
        else {
          BotWin(getPointOfGamer,getPointOfDealer)
        }
      }
      else if (getPointOfGamer <= 21 && getPointOfDealer > 21) {
        YouWin(getPointOfGamer,getPointOfDealer)
      }
      else {
        if (getPointOfGamer > getPointOfDealer) {
          YouWin(getPointOfGamer,getPointOfDealer)
        }
        else {
          BotWin(getPointOfGamer,getPointOfDealer)
        }
      }
    }

    /**
    *   玩家赢
      */
    def YouWin(PointOfGamer : Int,PointOfDealer : Int): Unit =
      {
        println("The Winner is: You")
        this.DisplayYourPoker
        this.DisplayBotPoker
        println("Your Point is: " + PointOfGamer)
        println("Bot Point is: " + PointOfDealer)
      }

    /**
    *   电脑赢
      */
    def BotWin(PointOfGamer : Int,PointOfDealer : Int): Unit =
      {
        println("The Winner is: Bot")
        this.DisplayYourPoker
        this.DisplayBotPoker
        println("Your Point is: " + PointOfGamer)
        println("Bot Point is: " + PointOfDealer)
      }

    /**
    *   展示玩家的牌
      */
    def DisplayYourPoker: Unit = {
      println("Now, You have ")
      for(i <- 0 until NumOfGamerPoker)
        println(PokerName(Gamer(i)))
    }

    def DisplayBotPoker: Unit = {
      println("Now, Bot have ")
      for (i <- 0 until NumOfDealerPoker)
        println(PokerName(Dealer(i)))
    }
 }

object NewTwentyOnePointGame
{
  def main(args: Array[String]): Unit = {

    println("Welcome to TwentyOnePoint Games!")
    var ContinueTheGame : Int = 1
    var Choose : Int = 0
    var BotsChoose : Int = 0
    var EndOfGame = false
    var BoundOfDealerPoint = 0
    var GamerLastCall = true                                      //玩家上一轮是否要牌 根据规则上一轮没要 后面不能再要
    var DealerLastCall = true
    var BlackJackChoose : Int = 0
    while(ContinueTheGame == 1) {
      GamerLastCall = true                                      //玩家上一轮是否要牌 根据规则上一轮没要 后面不能再要
      DealerLastCall = true
      println("Input Your Choose: 1.Continue the Game 2.Quit")
      ContinueTheGame = StdIn.readInt()
      if (ContinueTheGame == 1) {
        println("Begin The Game!")
        Choose = 0
        BotsChoose = 0
        EndOfGame = false
        val NewGame = new TwentyOnePointGame()
        NewGame.BeginNewGame()
        if(NewGame.GamerContainBlackJack == true) {
          NewGame.DisplayYourPoker
          println("Your have a BlackJack,Please choose :  1.As One ,2.As Eleven")
          BlackJackChoose = StdIn.readInt()
          NewGame.setBlackJackChoose(BlackJackChoose)
        }
        if (NewGame.getPointOfGamer == 21) {             //发完牌就是21点
          println("玩家满点!游戏结束!")
          NewGame.WhoWinTheGame()
          EndOfGame = true
        }
        if (NewGame.getPointOfDealer == 21) {
          println("电脑满点!游戏结束!")
          NewGame.WhoWinTheGame()
          EndOfGame = true
        }
        while (!EndOfGame) {
          NewGame.DisplayYourPoker
          if(GamerLastCall == true) {
            println("Input Your Choose : ")
            println("1.Call a Card 2.Don't call")
            Choose = StdIn.readInt()
            /**
              * 玩家要牌
              */
            if (Choose == 1) {
              println("玩家要牌")
              NewGame.GamerAskForPoker()
              if(NewGame.GamerContainBlackJack == true) {
                NewGame.DisplayYourPoker
                println("Your have a BlackJack,Please choose :  1.As One ,2.As Eleven")
                BlackJackChoose = StdIn.readInt()
                NewGame.setBlackJackChoose(BlackJackChoose)
              }

              /**
              *  玩家满点，游戏结束
                */
              if (NewGame.getPointOfGamer == 21) {
                println("玩家满点!游戏结束!")
                NewGame.WhoWinTheGame()
                EndOfGame = true
              }

              /**
                * 玩家牌爆，游戏结束
                */
              if (NewGame.getPointOfGamer > 21) {
                println("玩家牌爆了!游戏结束!")
                NewGame.WhoWinTheGame()
                EndOfGame = true
              }
            }
            else if (Choose == 2){
              println("玩家选择停牌")
              GamerLastCall = false
            }
            else
              {
                println("Input error!")
              }
          }
          if(GamerLastCall == false)
          {
            println("你已经停牌了")
          }

          /**
          *   玩家牌未爆且玩家没要牌但是电脑要了
            */

          BoundOfDealerPoint = NewGame.getPointOfDealer
          if(EndOfGame == false) {
            if (DealerLastCall == true) {
              if (BoundOfDealerPoint < 17) // 第二版这里我们让电脑变智能一点了 当他的分数小于17时 他肯定要牌 大于等于17时 随机决定是否要牌
              {
                BotsChoose = 1
                NewGame.DealerAskForPoker()
                println("电脑要牌")
                if (NewGame.getPointOfDealer == 21) {
                  println("电脑满点!游戏结束!")
                  NewGame.WhoWinTheGame()
                  EndOfGame = true
                }
                if (NewGame.getPointOfDealer > 21) {
                  println("电脑牌爆了!游戏结束!")
                  NewGame.WhoWinTheGame()
                  EndOfGame = true
                }
              }
              else if (BoundOfDealerPoint >= 17 && BoundOfDealerPoint < 21 )
              {
                BotsChoose = (new util.Random()).nextInt(2) // 此处电脑是否要牌是随机决定的 想不到更好地方法 如果电脑自行决定是否要牌 那就是AI了
                if(BotsChoose == 0)
                  {
                    println("电脑停牌")
                    DealerLastCall = false
                  }
                if (Choose == 2 && BotsChoose == 1) {
                  NewGame.DealerAskForPoker()
                  println("你已经选择停牌但电脑还在要牌")
                  if (NewGame.getPointOfDealer == 21) {
                    println("电脑满点!游戏结束!")
                    NewGame.WhoWinTheGame()
                    EndOfGame = true
                  }
                  else if (NewGame.getPointOfDealer > 21) {
                    println("电脑牌爆了!游戏结束!")
                    NewGame.WhoWinTheGame()
                    EndOfGame = true
                  }
                }
              }
            }
            else
              {
                println("电脑已经停牌了")
              }
          }

          /**
            *   如果玩家和电脑都停牌且都未爆牌 或是双方手牌已满时牌局结束
              */
          if(EndOfGame == false) {
            if ((Choose == 2 && BotsChoose == 0) || (NewGame.getNumOfGamerPoker == 5 && NewGame.getNumOfDealerPoker == 5)) {
              println("你和电脑都停牌或者你们的牌都满了,游戏结束")
              NewGame.WhoWinTheGame()
              EndOfGame = true
            }
          }
        }
      }
      else if (ContinueTheGame == 2){}
      else { println ("Input error!")}
  }
}
}






