package org.semantic.lang

import syntax.parsing.MatlabParser

/**
 * @author ilyas
 */

object Test extends MatlabParser {
  def main(args: Array[String]) {
    val in = """
    threshold = 0.3;
    l = length(swf)/2;
    af = abs(swf(1:l));
    %[M, res] = max(af);
    bool = 1;

    % Upper and lower monotony bounds around found maximum peak
    ub = res;
    lb = res;
    while (ub <= l && af(ub+1) < af(ub))
        ub = ub + 1;
    end
    while (lb > 1 && af(lb-1) < af(lb))
        lb = lb - 1;
    end

    for f = max(floor(res/3), 1):length(af),
      if (res == 1 || threshold*M <= af(f) && (f < lb || f > ub) && not(mod(f, res) < 2 || res - mod(f, res) < 2))
          bool = 0;
          return;
      end
    end

    res = res - 1;
    """
    val result = parse(in)
    println(result)
  }
}