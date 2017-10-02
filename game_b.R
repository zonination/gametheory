##########
# Game B #
##########

# Separate game file into different responses
game.b<-subset(game, !is.na(choice.b) & user!="zonination")[,c(3, 6)]

# Perform a check for duplicate entries
# game.b[duplicated(game.b$user)|duplicated(game.b$user, fromLast=T),]

# Check to see if there are even or odd columns.
# I'll enter to complete the pairing if there is odd number.
if(nrow(game.b) %% 2 != 0){
  game.b<-rbind(game.b, subset(game, !is.na(choice.b) & user=="zonination")[,c(3, 6)])}

# Begin by randomizing the sample set, and matching opponent.
game.b<-sample_n(game.b, nrow(game.b))
a<-game.b[1:(nrow(game.b)/2),]
b<-game.b[(nrow(game.b)/2 + 1):nrow(game.b),]
game.b<-cbind(a,b); rm(a); rm(b)
names(game.b)<-c("a.user", "a.choice",
                 "b.user", "b.choice")

# What is our end result?
game.b$end<-NA # Clear column
# Map out one of our four scenarios
for(n in 1:nrow(game.b)){
  # Both choose safe
  if(game.b$a.choice[n] == "Stay quiet" &
     game.b$b.choice[n] == "Stay quiet"){
    game.b$end[n] <- paste("Both /u/", game.b$a.user[n]," and /u/", game.b$b.user[n], " share a cell for one year.", sep="")}
  
  # A chooses risky and B chooses safe
  if(game.b$a.choice[n] == "Snitch" &
     game.b$b.choice[n] == "Stay quiet"){
    game.b$end[n] <- paste("/u/", game.b$a.user[n]," gets freed for selling out /u/", game.b$b.user[n], ", who got 3 years.", sep="")}
  
  # A chooses safe and B chooses risky
  if(game.b$a.choice[n] == "Stay quiet" &
     game.b$b.choice[n] == "Snitch"){
    game.b$end[n] <- paste("/u/", game.b$b.user[n]," gets freed for selling out /u/", game.b$a.user[n], ", who got 3 years.", sep="")}
  
  # Both choose risky
  if(game.b$a.choice[n] == "Snitch" &
     game.b$b.choice[n] == "Snitch"){
    game.b$end[n] <- paste("Both /u/", game.b$a.user[n]," and /u/", game.b$b.user[n], " share a cell for two years.", sep="")}
}; rm(n)

# Save the file; easy formatting for reddit tables
write_delim(game.b, "game_b.txt", delim="|")
write_csv(game.b, "game_b.csv")

# Plot and save the results
df<-data.frame("y"=c("Stay quiet","Snitch"), "x"=c("Snitch","Stay quiet"))
ggplot(game.b, aes(x=a.choice, y=b.choice))+
  geom_path(data=df, aes(x,y, group=1), lty=2)+
  geom_point(stat="bin2d", aes(size=..count..), shape=21, color="black", fill="steelblue1")+
  geom_text(stat="bin2d", aes(label=..count..))+
  scale_size(range=c(0,25), limits=c(0,120))+
  guides(size=F)+
  labs(title="The Prisoner's Dilemma",
       subtitle="Result matrix of scenario",
       x="Subject A",y="Subject B",
       caption="created by /u/zonination")+
  z_theme()
ggsave("game_b.png", height=4, width=5, dpi=120, type="cairo-png")