library(ggplot2)
library(gridExtra)
cbPalette <- c("#999999", "#E69F00", "#56B4E9")

frame_red1 <- frame_red[(frame_red$bandwidth == 1),]
tplot1 <- ggplot(data = frame_red1,aes(frame_red1$queue,frame_red1$performance/100000)) +
  geom_point(aes(colour = frame_red1$delay ), size = 2)+xlab("queue size") +
  ylab("performance") +ggtitle("bw =1")

frame_red2 <- frame_red[(frame_red$bandwidth == 4),]
tplot2 <- ggplot(data = frame_red2,aes(frame_red2$queue,frame_red2$performance/100000)) + 
  geom_point(aes(colour = frame_red2$delay), size = 2)+xlab("queue size") +
  ylab("performance") +ggtitle("bw =4")

frame_red3 <- frame_red[(frame_red$bandwidth == 7),]
tplot3 <- ggplot(data = frame_red3,aes(frame_red3$queue,frame_red3$performance/100000))+ 
  geom_point(aes(colour = frame_red3$delay), size = 2)+xlab("queue size") +
  ylab("performance") +ggtitle("bw =7")


frame_t1 <- frame_trickle[(frame_trickle$bandwidth ==1),]
tplot4 <- ggplot(data = frame_t1,aes(frame_t1$queue,frame_t1$performance/100000)) +
  geom_point(aes(colour = frame_t1$delay ), size = 2)+xlab("queue size") +
  ylab("performance") +ggtitle("droptail/bw =1")

frame_t2 <- frame_trickle[(frame_trickle$bandwidth ==4),]
tplot5 <- ggplot(data = frame_t2,aes(frame_t2$queue,frame_t2$performance/100000)) + 
  geom_point(aes(colour = frame_t2$delay ), size = 2)+xlab("queue size") +
  ylab("performance") +ggtitle("droptail/bw =4")

frame_t3 <- frame_trickle[(frame_trickle$bandwidth ==7),]
tplot6 <- ggplot(data = frame_t3,aes(frame_t3$queue,frame_t3$performance/100000)) + 
  geom_point(aes(colour = frame_t3$delay ), size = 2)+xlab("queue size") +
  ylab("performance") +ggtitle("droptail/bw =7")

grid.arrange(tplot1,tplot2,tplot3)
grid.arrange(tplot4,tplot5,tplot6)
grid.arrange(tplot1,tplot4)
grid.arrange(tplot2,tplot5)
grid.arrange(tplot3,tplot6)
grid.arrange(tplot1,tplot4,tplot2,tplot5,tplot3,tplot6)
