package org.scala.mt;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;

public class RateLimiting {
    public static void main(String[] args) {
        List<Long> myLIst = new LinkedList<>();
        while(true) {
            try {
                Thread.sleep(1000);
            } catch (Exception e) {

            }

            if(apiLimitExceeded(myLIst,System.currentTimeMillis())){
                System.out.println("you have exceeded the API rate limit");
                try{Thread.sleep(2000);}
                catch (Exception e) {}
            } else {
                System.out.println("making calls to the API");
            }
        }
    }

    public static boolean apiLimitExceeded(List<Long> myQ,long ts) {
        System.out.println(myQ);
        if(myQ.size() <  5) {
            myQ.add(ts);
            return false;
        } else {
            if(ts - myQ.get(myQ.size()-5) >= 1000) {
                myQ.add(ts);
                return true;
            } else {
                myQ.add(ts);
                System.out.println(myQ);
                return false;
            }
        }
    }
}
