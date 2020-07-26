#### MoCA change (for figure 2 (trajectory map) and tables 3 and 4)
add_moca_trends <- function (data) {
  
  moca_with_trends <- data %>%
    mutate(trends =
             ifelse(
               moca_score_mo12 > moca_score_mo3 &
                 moca_score_mo3 > moca_score_w1,
               '1.overall improver',
               ifelse(moca_score_mo3 > moca_score_w1 &
                        moca_score_mo3 == moca_score_mo12,
                      '2.improved-stable',
                      ifelse(moca_score_mo3 > moca_score_w1 &
                               moca_score_mo12 < moca_score_mo3,
                             '3.improved-declined',
                             ifelse(moca_score_mo3 == moca_score_w1 &
                                      moca_score_mo12 > moca_score_mo3 ,
                                    '4.stable-improved',
                                    ifelse(moca_score_mo12 == moca_score_mo3 &
                                             moca_score_mo3 == moca_score_w1,
                                           '5.overall stable',
                                           ifelse(moca_score_mo3 == moca_score_w1 &
                                                    moca_score_mo12 < moca_score_mo3,
                                                  '6.stable-declined',
                                                  ifelse(moca_score_mo3 < moca_score_w1 &
                                                           moca_score_mo12 > moca_score_mo3,
                                                         '7.declined-improved',
                                                         ifelse(moca_score_mo3 < moca_score_w1 &
                                                                  moca_score_mo3 == moca_score_mo12,
                                                                '8.declined-stable',
                                                                ifelse(moca_score_mo12 < moca_score_mo3 &
                                                                         moca_score_mo3 < moca_score_w1,
                                                                       '9.overall decliner',NA))))))))))
  
  
}
