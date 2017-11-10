package tys.frontier.style.order;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import tys.frontier.code.FClassMember;
import tys.frontier.code.FClassMember.MemberType;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.util.EnumComparator;

import java.util.Comparator;

public class Order {

    public static final Comparator<FClassMember> DEFAULT =
            ByMemberType.DEFAULT
                    .thenComparing(ByStatic.PREFER_STATIC)
                    .thenComparing(ByVisibility.DEFAULT)
                    .thenComparing(Alphabetical.INSTANCE);

    public static Comparator<FClassMember> fromJSON(JSONArray order) {
        Comparator<FClassMember> res = null;
        for(int i=0; i<order.length(); i++) {
            try {
                Comparator<FClassMember> comp;
                JSONObject o = order.getJSONObject(i);
                String by = o.getString("by");
                switch (by) {
                    case "type":
                        comp = byMemberTypeFromJSON(o.getJSONArray("order"));
                        break;
                    case "static":
                        comp = byStaticFromJSON(o.getJSONArray("order"));
                        break;
                    case "visibility":
                        comp = byVisibilityFromJSON(o.getJSONArray("order"));
                        break;
                    case "alphabet":
                        comp = Alphabetical.INSTANCE;
                        break;
                    default:
                        continue;
                }

                if (res == null)
                    res = comp;
                else
                    res = res.thenComparing(comp);
            } catch (JSONException ignored) {}
        }
        return res;
    }

    public static ByMemberType byMemberTypeFromJSON(JSONArray order) {
        EnumComparator.Builder<MemberType> builder = new EnumComparator.Builder<>(MemberType.class);
        for(int i=0; i<order.length(); i++) {
            try {
                String s = order.getString(i);
                builder.next(MemberType.valueOf(s.toUpperCase()));
            } catch (JSONException | NullPointerException | IllegalArgumentException ignored) {}
        }
        return new ByMemberType(builder.build());
    }

    public static ByStatic byStaticFromJSON(JSONArray order) {
        for(int i=0; i<order.length(); i++) {
            try {
                String s = order.getString(i);
                switch (s) {
                    case "static":
                        return ByStatic.PREFER_STATIC;
                    case "instance":
                        return ByStatic.PREFER_INSTANCE;
                }
            } catch (JSONException ignored) {}
        }
        throw new JSONException("");
    }

    public static ByVisibility byVisibilityFromJSON(JSONArray order) {
        EnumComparator.Builder<FVisibilityModifier> builder = new EnumComparator.Builder<>(FVisibilityModifier.class);
        for(int i=0; i<order.length(); i++) {
            try {
                String s = order.getString(i);
                builder.next(FVisibilityModifier.valueOf(s.toUpperCase()));
            } catch (JSONException | NullPointerException | IllegalArgumentException ignored) {}
        }
        return new ByVisibility(builder.build());
    }




    private Order () {}

}
