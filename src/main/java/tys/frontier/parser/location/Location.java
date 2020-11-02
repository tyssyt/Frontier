package tys.frontier.parser.location;

import java.nio.file.Path;

public class Location {

    private Path file;
    private Position position;

    public Location(Path file, Position position) {
        this.file = file;
        this.position = position;
    }

    public Path getFile() {
        return file;
    }

    public Position getPoint() {
        return position;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Location location = (Location) o;

        if (!file.equals(location.file)) return false;
        return position.equals(location.position);
    }

    @Override
    public int hashCode() {
        int result = file.hashCode();
        result = 31 * result + position.hashCode();
        return result;
    }

    @Override
    public String toString() {
        return "Location{" + "file=" + file + ", point=" + position + '}';
    }
}
